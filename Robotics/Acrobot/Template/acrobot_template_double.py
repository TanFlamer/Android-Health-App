# Adapted from: https://medium.com/@tuzzer/cart-pole-balancing-with-q-learning-b54c6068d947

import results_processing
import math
import random
import gym
import numpy as np

# Initialize the "Acrobot" environment
env = gym.make('Acrobot-v1')

# Bounds for each discrete state
STATE_BOUNDS = list(zip(env.observation_space.low, env.observation_space.high))

# Defining the simulation related constants
NUM_TRAIN_EPISODES = 1000
MAX_TRAIN_T = 500


def loop(run_settings, hyper_parameters, variables):
    min_runs, max_runs = run_settings
    episodes = []
    failed_runs = []
    run_number = 0
    while len(episodes) < min_runs and run_number < max_runs:
        run_number += 1
        episode, solved = train(run_number, hyper_parameters, variables)
        if solved:
            episodes.append(episode)
        else:
            failed_runs.append(run_number)
            print("Run %2d failed in %d episodes - *" % (run_number, NUM_TRAIN_EPISODES))
    return results_processing.get_results(episodes), failed_runs


def train(run, hyper_parameters, variables):

    # Unpacking hyper parameters
    discount_factor, min_learning_rate, min_explore_rate = hyper_parameters

    # Unpacking variables
    num_buckets, num_actions, q_table_settings, termination_penalty, opposite_penalty = variables

    # Instantiating the learning related parameters
    learning_rate = get_learning_rate(0, min_learning_rate)
    explore_rate = get_explore_rate(0, min_explore_rate)

    q_table_a = np.zeros(num_buckets + (num_actions,)) if q_table_settings[0] == 0 else np.random.randn(
        *num_buckets, num_actions) * q_table_settings[0] + q_table_settings[1]
    q_table_b = np.zeros(num_buckets + (num_actions,)) if q_table_settings[0] == 0 else np.random.randn(
        *num_buckets, num_actions) * q_table_settings[0] + q_table_settings[1]

    time_steps = [0]
    episodes_to_solve = 0
    solved = False

    for episode in range(1, NUM_TRAIN_EPISODES + 1):

        # Reset the environment
        obv, _ = env.reset()

        # the initial state
        state_0 = state_to_bucket(obv, num_buckets)

        for t in range(1, MAX_TRAIN_T + 1):
            env.render()

            # Select an action
            action = select_action(state_0, explore_rate, q_table_a + q_table_b, num_actions)

            # Execute the action
            obv, reward, terminated, _, _ = env.step(action if num_actions == 3 else action * 2)

            # Observe the result
            state = state_to_bucket(obv, num_buckets)

            if terminated:
                state_information = (state_0, state, action, reward)
                parameters = (learning_rate, discount_factor, opposite_penalty)
                termination = (terminated, termination_penalty)
                update_q_table(q_table_a, q_table_b, state_information, parameters, termination)
                time_steps.append(t + time_steps[-1])
                break

            # Update the Q based on the result
            state_information = (state_0, state, action, reward)
            parameters = (learning_rate, discount_factor, opposite_penalty)
            termination = (terminated, termination_penalty)
            update_q_table(q_table_a, q_table_b, state_information, parameters, termination)

            # Setting up for the next iteration
            state_0 = state

        else:
            time_steps.append(MAX_TRAIN_T + time_steps[-1])

        # It's considered done when average for last 100 time steps is <= 150.0
        average = results_processing.get_average(time_steps)

        if episode % 50 == 0:
            print("%d %f" % (episode, average))

        if average <= 195.0:
            episodes_to_solve = episode
            solved = True
            print("Run %2d solved in %d episodes" % (run, episode))
            break

        # Update parameters
        learning_rate = get_learning_rate(episode, min_learning_rate)
        explore_rate = get_explore_rate(episode, min_explore_rate)

    # print(q_table)
    return episodes_to_solve, solved


def update_q_table(q_table_a, q_table_b, state_information, parameters, termination):
    state_0, state, action, reward = state_information
    learning_rate, discount_factor, opposite_penalty = parameters
    terminated, termination_penalty = termination

    opposite_action = (action + 1) % 2
    reward = termination_penalty if terminated else reward

    if random.random() < 0.5:
        # Update A
        q_table_a[state_0 + (action,)] += learning_rate * (reward + discount_factor * q_table_b[
            state + (np.argmax(q_table_a[state]),)] - q_table_a[state_0 + (action,)])
        q_table_a[state_0 + (opposite_action,)] += opposite_penalty
    else:
        # Update B
        q_table_b[state_0 + (action,)] += learning_rate * (reward + discount_factor * q_table_a[
            state + (np.argmax(q_table_b[state]),)] - q_table_b[state_0 + (action,)])
        q_table_b[state_0 + (opposite_action,)] += opposite_penalty


def select_action(state, explore_rate, q_table, num_actions):
    # Select a random action
    if random.random() < explore_rate:
        action = env.action_space.sample() if num_actions == 3 else np.random.randint(0, 2)
    # Select the action with the highest q
    else:
        action = np.argmax(q_table[state])
    return action


def get_explore_rate(t, min_explore_rate):
    return max(min_explore_rate, min(1.0, 1.0 - math.log10((t + 1) / 25)))


def get_learning_rate(t, min_learning_rate):
    return max(min_learning_rate, min(0.5, 1.0 - math.log10((t + 1) / 25)))


def state_to_bucket(state, num_buckets):
    bucket_indices = []
    for i in range(len(state)):
        if state[i] <= STATE_BOUNDS[i][0]:
            bucket_index = 0
        elif state[i] >= STATE_BOUNDS[i][1]:
            bucket_index = num_buckets[i] - 1
        else:
            # Mapping the state bounds to the bucket array
            bound_width = STATE_BOUNDS[i][1] - STATE_BOUNDS[i][0]
            offset = (num_buckets[i] - 1) * STATE_BOUNDS[i][0] / bound_width
            scaling = (num_buckets[i] - 1) / bound_width
            bucket_index = int(round(scaling * state[i] - offset))
            # For easier visualization of the above, you might want to use
            # pen and paper and apply some basic algebraic manipulations.
            # If you do so, you will obtain (B-1)*[(S-MIN)]/W], where
            # B = NUM_BUCKETS, S = state, MIN = STATE_BOUNDS[i][0], and
            # W = bound_width. This simplification is very easily
            # to visualize, i.e. num_buckets x percentage in width.
        bucket_indices.append(bucket_index)
    return tuple(bucket_indices)


def random_seed(seed):
    np.random.seed(seed)
    random.seed(seed)
    env.action_space.seed(seed)
    env.reset(seed=seed)


def run_simulation(seed_random, run_settings, hyper_parameters, variables):
    random_seed(seed_random)
    results, failed = loop(run_settings, hyper_parameters, variables)
    results_processing.print_results(results, failed)
