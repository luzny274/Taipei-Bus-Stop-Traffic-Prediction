import torch
import torchvision
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import torch.optim.lr_scheduler as lr_scheduler

import numpy as np
import matplotlib.pyplot as plt
import copy
import time

import preprocess

##Initialize model weights
def weights_init(layer_in):
  if isinstance(layer_in, nn.Linear):
    torch.nn.init.xavier_uniform_(layer_in.weight, gain=0.5)
    layer_in.bias.data.fill_(0.0)

## Turn data into sequences that can be used for training
def get_batch_sequences(batch_idx, batch_size, permutation, X, Y = None):
    sample_sz = X.shape[0]

    batch_start = batch_idx * batch_size
    batch_end   = min(sample_sz, (batch_idx + 1) * batch_size)
    actual_size = batch_end - batch_start

    batch_indeces = permutation[batch_start:batch_end]
    batch_target = None # In case that we just want to batch X, we set batch_target to None
    if Y != None:
        batch_target = Y[batch_indeces]
    batch_data = X[batch_indeces]

    return [batch_data, batch_target, actual_size]

## Training step
def train(epoch, model, optimizer, X, Y, batch_size, loss_fnc, l1_regularization, print_mode, scheduler = None):
    model.train()

    sample_sz = X.shape[0]
    perm = np.random.permutation(sample_sz)

    total_loss = 0
    for batch_idx in range(0, int(np.ceil(sample_sz / batch_size))):
        batch_data, batch_target, actual_size = get_batch_sequences(batch_idx, batch_size, perm, X, Y)
        optimizer.zero_grad()

        output = model(batch_data)
        loss = loss_fnc(output, batch_target, reduction="mean") 
        loss += l1_regularization * torch.sum(torch.abs(model.get_input_weights()))
        total_loss += loss.item()

        loss.backward()
        optimizer.step()
        if scheduler != None:
            scheduler.step()
        if print_mode:
            print('Epoch:{} \tTrain loss: {:.7f}'.format(epoch, total_loss / (batch_idx + 1)), end="\r")

    total_loss /= np.ceil(sample_sz / batch_size)
    if not print_mode:
        print('Epoch:{} \tTrain loss: {:.7f}'.format(epoch, total_loss), end="\r")

    return total_loss

## Testing
def test(model, X, Y, batch_size, loss_fnc):
    model.eval()
    test_loss = 0

    sample_sz = X.shape[0]
    output_sz = Y.shape[1]
    with torch.no_grad():
        perm = np.random.permutation(sample_sz)

        for batch_idx in range(0, int(np.ceil(sample_sz / batch_size))):
            batch_data, batch_target, actual_size = get_batch_sequences(batch_idx, batch_size, perm, X, Y)

            output = model(batch_data)
            test_loss += loss_fnc(output, batch_target, reduction="sum").item()

    test_loss /= (sample_sz * output_sz)
    return test_loss

## Predict
def predict(model, X, batch_size):
    model.eval()
    test_loss = 0

    sample_sz = X.shape[0]
    Y_hat = None

    with torch.no_grad():
        perm = np.arange(0, sample_sz)

        for batch_idx in range(0, int(np.ceil(sample_sz / batch_size))):
            batch_data, batch_target, actual_size = get_batch_sequences(batch_idx, batch_size, perm, X)

            output = model(batch_data)

            start_idx = batch_idx * batch_size
            end_idx = start_idx + actual_size

            if Y_hat is None:
                output_size = output[:].cpu().shape[1]
                Y_hat = np.zeros((sample_sz, output_size))
            Y_hat[start_idx:end_idx] = output[:].cpu()


    return Y_hat
    
class Utils:
    ## Initialize PyTorch and move the dataset to 
    def __init__(self, dataset, use_cuda = True):
        default_dtype = torch.float32
        torch.set_default_dtype(default_dtype)

        # Initialize PyTorch
        cuda_available = torch.cuda.is_available()
        self.USE_CUDA = use_cuda and cuda_available
        print("Cuda available: ", cuda_available)
        print("Using Cuda:     ", self.USE_CUDA)

        if cuda_available:
            print(torch.cuda.get_device_name(0))
            cuda_device = torch.device("cuda:0")

        # If the training data contains time series, then we need slighly change the conversions
        do_a_time_series = isinstance(dataset.X_train, preprocess.TimeSeries)

        self.dataset = dataset

        # Move the dataset to GPU
        if do_a_time_series:
            self.X_train_t = dataset.X_train.to_pytorch(use_cuda = self.USE_CUDA, dtype = default_dtype)
            self.X_val_t   = dataset.X_val  .to_pytorch(use_cuda = self.USE_CUDA, dtype = default_dtype)
            self.X_test_t  = dataset.X_test .to_pytorch(use_cuda = self.USE_CUDA, dtype = default_dtype)
        else:
            self.X_train_t = torch.from_numpy(dataset.X_train).to(dtype = default_dtype)
            self.X_val_t   = torch.from_numpy(dataset.X_val  ).to(dtype = default_dtype)
            self.X_test_t  = torch.from_numpy(dataset.X_test ).to(dtype = default_dtype)
        self.Y_train_t = torch.from_numpy(dataset.Y_train).to(dtype = default_dtype)
        self.Y_val_t   = torch.from_numpy(dataset.Y_val  ).to(dtype = default_dtype)
        self.Y_test_t  = torch.from_numpy(dataset.Y_test ).to(dtype = default_dtype)

        if self.USE_CUDA:
            if not do_a_time_series:
                self.X_train_t = self.X_train_t.cuda()
                self.X_val_t   = self.X_val_t.cuda()
                self.X_test_t  = self.X_test_t.cuda()
            self.Y_train_t = self.Y_train_t.cuda()
            self.Y_val_t   = self.Y_val_t.cuda()
            self.Y_test_t  = self.Y_test_t.cuda()

        # Dictionary to map strings to loss functions
        self.loss_functions = {
            'SmoothL1': F.smooth_l1_loss,
            'L1': F.l1_loss,
            'MSE': F.mse_loss
        }

    ## The whole training, mode : {0 : "No printing and no saving", 1 : "Print info through the training and capture the model with minimum val loss"}
    def train_model(self, model_class, params, max_batch_size, print_mode):
        # Initialize model
        model = model_class(params)
        if self.USE_CUDA:
            model = model.cuda()
            model.apply(weights_init)

        # Get training parameters
        loss_fnc = self.loss_functions[params["loss"]]
        epochs = params["epochs"]
        lr = params["lr"]
        batch_size = params["batch_size"]
        weight_decay = params["weight_decay"]
        batch_growth = params["batch_growth"]
        l1_regularization = params["l1_reg"]

        # Get adam optimizer
        optimizer = optim.Adam(model.parameters(), lr=lr, weight_decay=weight_decay)

        # Measure time it takes for the model to train
        start_time = time.time()

        # Get initial performance
        test_loss = test(model, self.X_val_t, self.Y_val_t, batch_size, loss_fnc)
        if print_mode:
            print('\nInitial: Test loss: {:.7f}\n'.format(test_loss))


        # Initialize learning rate scheduler with cosine decay
        scheduler_steps = int(epochs * np.ceil(self.X_train_t.shape[0] / batch_size))
        if batch_growth:
            scheduler_steps = int((epochs // 2) * np.ceil(self.X_train_t.shape[0] / max_batch_size))
        scheduler = lr_scheduler.CosineAnnealingLR(optimizer, T_max=scheduler_steps, eta_min=0)

        # If batch growth is set to true, then batch size will increase for the first half of the training
        batch_increase_range = epochs - epochs // 2
        batch_increase_step_sz = batch_increase_range / np.log2(max_batch_size / batch_size)

        # Capturing the model in a state with minimum val loss through the training 
        best_model = model_class(params)    
        if self.USE_CUDA:
            best_model = best_model.cuda()
        best_val_loss = float("inf")
        
        # Train
        train_loss = None
        train_losses = []
        val_losses = []
        for epoch in range(1, epochs + 1):
            current_batch_size = None
            if batch_growth:
                # Including batch growth
                current_batch_size = np.round(batch_size * np.power(2, (epoch - 1) // batch_increase_step_sz))
                current_batch_size = int(min(current_batch_size, max_batch_size))
            else:
                # No batch growth
                current_batch_size = batch_size

            if (epoch > np.ceil(batch_increase_range)) or (not batch_growth): 
                # Training step with lr scheduling
                train_loss = train(epoch, model, optimizer, self.X_train_t, self.Y_train_t, current_batch_size, loss_fnc, l1_regularization, print_mode, scheduler)
            else:
                # Training step without scheduling
                train_loss = train(epoch, model, optimizer, self.X_train_t, self.Y_train_t, current_batch_size, loss_fnc, l1_regularization, print_mode)

            if print_mode:
                # Calculate the val loss and save to the list
                val_loss = test(model, self.X_val_t, self.Y_val_t, batch_size, loss_fnc)
                train_losses.append(train_loss)
                val_losses.append(val_loss)
                
                # Capturing the model in a state with minimum val loss through the training = Early stopping
                if val_loss < best_val_loss:
                    best_val_loss = val_loss
                    best_model.load_state_dict(model.state_dict())

                print('Epoch:' + str(epoch) + ' \tTrain loss: {:.7f} \tVal loss: {:.7f}'.format(train_loss, val_loss) + ' \tbatch size: {} \t LR: {}'.format(current_batch_size, optimizer.param_groups[0]['lr']))

        final_val_loss = test(model, self.X_val_t, self.Y_val_t, batch_size, loss_fnc)
        print("\nTime to train: {:.3f}s\t final train loss:{:.7f}\t final val loss:{:.7f}".format(time.time() - start_time, train_loss, final_val_loss))

        if print_mode:
            final_model = model
            # Return the best model, the best val loss, final trained model and a list of train and val losses in different epochs
            return [best_model, best_val_loss, final_model, train_losses, val_losses]   
        else:
            # Return the trained model with final train and val loss
            return [model, train_loss, final_val_loss] 

    ## Grid search
    def grid_search(self, model_class, param_variations, max_batch_size):
        best_val_loss = float("inf")
        best_train_loss = float("inf")
        best_val_params = None
        best_train_params = None

        val_losses = []
        train_losses = []

        # Iterate over all possible parameter combinations
        for i in range(len(param_variations)):
            params = param_variations[i]
            print("\nGrid search step {}/{}".format(i + 1, len(param_variations)), "\t| params:", params)

            # Train the model
            model, train_loss, val_loss = self.train_model(model_class, params, max_batch_size, print_mode=False)
            train_losses.append(train_loss)
            val_losses.append(val_loss)

            # Capture the model with minimum val loss
            if best_val_loss > val_loss:
                print("New val best!")
                best_val_loss = val_loss
                best_val_params = params
                
        print("\nBest validation loss: ", best_val_loss)
        print("Best val params: ", best_val_params)
        return [best_val_params, train_losses, val_losses]

    ## Evaluation
    def evaluate_model(self, model, batch_size):
        def print_metrics(model, X, Y, batch_size):
            Y_hat = predict(model, X, batch_size)

            MSE = np.mean((Y_hat - Y) ** 2)
            orig_MSE = np.mean((self.dataset.Y_inverse_transform(Y_hat) - self.dataset.Y_inverse_transform(Y)) ** 2)
            print("\tMSE:", MSE)
            print("\tOriginal scale MSE:", orig_MSE)

            print("\n\tRMSE:", np.sqrt(MSE))
            print("\tOriginal scale RMSE:", np.sqrt(orig_MSE))

            print("\n\tMAE:", np.mean(np.abs(Y_hat - Y)))
            print("\tOriginal scale MAE:", np.mean(np.abs(self.dataset.Y_inverse_transform(Y_hat) - self.dataset.Y_inverse_transform(Y))))

        print("Train metrics:")
        print_metrics(model, self.X_train_t, self.dataset.Y_train, batch_size)
        print("\nValidation metrics:")
        print_metrics(model, self.X_val_t, self.dataset.Y_val, batch_size)
        print("\nTest metrics:")
        print_metrics(model, self.X_test_t, self.dataset.Y_test, batch_size)
        
    ## Learning Rate Range Test
    def Learning_Rate_Range_Test(self, model_class, min_lr, max_lr, params):
        # Initialize model
        model = model_class(params)
        if self.USE_CUDA:
            model = model.cuda()
            model.apply(weights_init)

        # Get training parameters
        loss_fnc = self.loss_functions[params["loss"]]
        batch_size = params["batch_size"]
        weight_decay = params["weight_decay"]
        epochs = params["epochs"]

        step_cnt = epochs
        gamma = np.power((max_lr/min_lr), 1/step_cnt)
        print("gamma per epoch:", gamma)
        print("residual:", np.abs(max_lr/min_lr - np.power(gamma, step_cnt)))

        # Measure the time it takes to train the model
        start_time = time.time()

        # Get initial performance
        test_loss = test(model, self.X_train_t, self.Y_train_t, batch_size, loss_fnc)
        print('\nInitial: Train loss: {:.7f}\n'.format(test_loss))

        # Train
        train_loss = None
        train_losses = []
        learning_rates = []

        lr=min_lr

        for epoch in range(1, epochs + 1):
            # Initialize new model
            model_copy = model_class(params)        
            if self.USE_CUDA:
                model_copy = model_copy.cuda()
            model_copy.load_state_dict(model.state_dict())

            optimizer = optim.Adam(model_copy.parameters(), lr=lr, weight_decay=weight_decay)

            # Train for one epoch
            train_loss = train(epoch, model_copy, optimizer, self.X_train_t, self.Y_train_t, batch_size, loss_fnc, l1_regularization=0, print_mode=True)
            
            # Save training performance
            learning_rates.append(lr)
            train_losses.append(train_loss)
            print('Step:' + str(epoch) + ' \tTrain loss: {:.7f} \tLR:'.format(train_loss), learning_rates[-1])

            # Change learning rate for the next step
            lr*=gamma 
        train_losses = np.array(train_losses)
        learning_rates = np.array(learning_rates)

        print("\nTime to train: {:.3f}s".format(time.time() - start_time))

        # Get learning rate that minimizes loss the most
        min_loss_lr = learning_rates[np.argmin(train_losses)]
        
        # Plot train loss vs learning rate
        plt.plot(learning_rates, train_losses)
        plt.gca().set_xscale('log')
        plt.gca().set_yscale('log')
        plt.xlabel("learning rate")
        plt.ylabel("loss")
        plt.show()

        print("Best learning rate:", min_loss_lr)

        return [model, train_losses, learning_rates, min_loss_lr]
        
    ## Get maximum batch size
    def get_max_batch_size(self, model):
        max_batch_size = 2
        Y_train_hat = predict(model, self.X_train_t, max_batch_size)

        # Double batch size until data size is reached or OOM is thrown
        try:
            while max_batch_size < self.X_train_t.shape[0]:
                Y_train_hat = predict(model, self.X_train_t, max_batch_size)
                max_batch_size *= 2
            print("batch size of {} is larger than the dataset size!".format(max_batch_size))
        except:
            print("batch size of {} caused OOM!".format(max_batch_size))
        
        max_batch_size = int(max_batch_size / 2)
        print("max_batch_size:", max_batch_size)
        
        return max_batch_size

        
    ## Create list of parameter dictionaries for grid search, Generating every possible combination of input parameters
    def create_param_dict_from_lists(self, list_of_params, list_of_names):
        params = []

        feature_vals = list_of_params[0]
        feature_name = list_of_names[0]
        for feature_val in feature_vals:
            params_row = {}
            params_row[feature_name] = feature_val
            params.append(params_row)

        for i in range(1, len(list_of_params)):
            feature_vals = list_of_params[i]
            feature_name = list_of_names[i]

            old_params = params
            new_params = []
            for past_param in old_params:
                for feature_val in feature_vals:
                    params_row = copy.copy(past_param)
                    params_row[feature_name] = feature_val
                    new_params.append(params_row)
            params = new_params

        return params
        
    ## Grid search plotting
    def plot_grid(self, grid_values, title, x_label, y_label, x_values, y_values):
        plt.matshow(grid_values)
        plt.title(title)
        plt.xlabel(x_label)
        plt.ylabel(y_label)
        plt.colorbar()
        plt.xticks(list(range(len(x_values))), x_values)
        plt.yticks(list(range(len(y_values))), y_values)

        for (i, j), z in np.ndenumerate(grid_values):
            plt.text(j, i, '{:0.5f}'.format(z), ha='center', va='center')

        plt.show()

    ## Plot training progress
    def plot_losses(self, train_losses, val_losses, title):
        plt.title(title)
        plt.plot(train_losses, label="train loss")
        plt.plot(val_losses, label="val loss")
        plt.xlabel("epochs")
        plt.ylabel("MSE")
        plt.legend()
        plt.show()