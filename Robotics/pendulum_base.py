# Adapted from: https://medium.com/@tuzzer/cart-pole-balancing-with-q-learning-b54c6068d947

import math
import random
import gym
import numpy as np

# Initialize the "Cart-Pole" environment
env = gym.make('Pendulum-v1')

# Defining the environment related constants
# Number of discrete states (bucket) per state dimension
NUM_BUCKETS = (10, 1, 10)
# Number of discrete actions
NUM_ACTIONS = 32
# Bounds for each discrete state
STATE_BOUNDS = list(zip(env.observation_space.low, env.observation_space.high))

# Learning related constants
# Continue here
MIN_EXPLORE_RATE = 0.01
MIN_LEARNING_RATE = 0.1

# Defining the simulation related constants
NUM_TRAIN_EPISODES = 500  # 1000
MAX_TRAIN_T = 500


def loop():
    episodes = []
    failed_runs = []
    run_number = 0
    while len(episodes) < 30 and run_number < 50:
        run_number += 1
        episode, solved = train(run_number)
        if solved:
            episodes.append(episode)
        else:
            failed_runs.append(run_number)
            print("Run %2d failed in 500 episodes - *" % run_number)
    return get_results(episodes), failed_runs


def get_results(episodes):
    runs = len(episodes)
    if runs == 0:
        return 0, 0, 0, 0, 0, 0
    elif runs == 1:
        return episodes[0], 0, episodes[0], 0, episodes[0], episodes[0]
    else:
        mean = sum(episodes) / runs
        variance = (sum(np.square(episodes)) / runs) - (mean * mean)
        standard_deviation = math.sqrt(variance)

        episodes.sort()
        if runs % 2 == 0:
            midpoint = runs // 2 - 1
            median = (episodes[midpoint] + episodes[midpoint + 1]) / 2
            runs_halved = runs // 2
            offset = 0
        else:
            midpoint = (runs + 1) // 2 - 1
            median = episodes[midpoint]
            runs_halved = (runs - 1) // 2
            offset = 1

        if runs_halved % 2 == 0:
            midpoint = runs_halved // 2 - 1
            print(midpoint)
            first_quartile = (episodes[midpoint] + episodes[midpoint + 1]) / 2
            third_quartile = (episodes[midpoint + runs_halved + offset] + episodes[
                midpoint + runs_halved + offset + 1]) / 2
        else:
            midpoint = (runs_halved + 1) // 2 - 1
            first_quartile = episodes[midpoint]
            third_quartile = episodes[midpoint + runs_halved + offset]
        inter_quartile_range = third_quartile - first_quartile

        return mean, standard_deviation, median, inter_quartile_range, max(episodes), min(episodes)


def train(run):
    # Instantiating the learning related parameters
    learning_rate = get_learning_rate(0)
    explore_rate = get_explore_rate(0)
    discount_factor = 0.999  # since the world is unchanging

    q_table = np.zeros(NUM_BUCKETS + (NUM_ACTIONS,))
    time_steps = [0]
    episodes_to_solve = 0
    solved = False

    for episode in range(1, NUM_TRAIN_EPISODES + 1):

        # Reset the environment
        obv, _ = env.reset()

        # the initial state
        state_0 = state_to_bucket(obv)

        rewards = 0

        for t in range(1, MAX_TRAIN_T + 1):
            env.render()

            # Select an action
            action = select_action(state_0, explore_rate, q_table)

            # Execute the action
            obv, reward, _, truncated, _ = env.step(action)

            rewards += reward

            # Observe the result
            state = state_to_bucket(obv)

            # Update the Q based on the result
            action_bucket = round((action[0] + 2.0) / (4.0 / NUM_ACTIONS) - 0.5)
            best_q = np.amax(q_table[state])
            q_table[state_0 + (action_bucket,)] += learning_rate * (
                    reward + discount_factor * best_q - q_table[state_0 + (action_bucket,)])

            # Setting up for the next iteration
            state_0 = state

            if truncated:
                # print("Episode %d finished after %f time steps" % (episode, t))
                time_steps.append(reward + time_steps[-1])
                break

        # It's considered done when average for last 100 time steps is >= 195.0
        num_elements = len(time_steps) - 1
        if num_elements <= 100:
            average = time_steps[-1] / num_elements
        else:
            average = (time_steps[-1] - time_steps[-101]) / 100

        if episode % 100 == 0:
            print("%d %f" % (episode, average))

        if average >= -5 and episode >= 100:
            episodes_to_solve = episode
            solved = True
            print("Run %2d solved in %d episodes" % (run, episode))
            break

        # Update parameters
        explore_rate = get_explore_rate(episode)
        learning_rate = get_learning_rate(episode)

    # print(q_table)
    return episodes_to_solve, solved


def select_action(state, explore_rate, q_table):
    # Select a random action
    if random.random() < explore_rate:
        action = env.action_space.sample()
    # Select the action with the highest q
    else:
        action = [(np.argmax(q_table[state]) + 0.5) * (4.0 / NUM_ACTIONS) - 2.0]
    return action


def get_explore_rate(t):
    return max(MIN_EXPLORE_RATE, min(1.0, 1.0 - math.log10((t + 1) / 25)))


def get_learning_rate(t):
    return max(MIN_LEARNING_RATE, min(0.5, 1.0 - math.log10((t + 1) / 25)))


def state_to_bucket(state):
    bucket_indices = []
    for i in range(len(state)):
        if state[i] <= STATE_BOUNDS[i][0]:
            bucket_index = 0
        elif state[i] >= STATE_BOUNDS[i][1]:
            bucket_index = NUM_BUCKETS[i] - 1
        else:
            # Mapping the state bounds to the bucket array
            bound_width = STATE_BOUNDS[i][1] - STATE_BOUNDS[i][0]
            offset = (NUM_BUCKETS[i] - 1) * STATE_BOUNDS[i][0] / bound_width
            scaling = (NUM_BUCKETS[i] - 1) / bound_width
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


def print_results(result, failed_runs):
    print("")
    print("Mean = %f" % result[0])
    print("Standard Deviation = %f" % result[1])
    print("Median = %.1f" % result[2])
    print("Inter-Quartile Range = %.1f" % result[3])
    print("Max = %d" % result[4])
    print("Min = %d" % result[5])
    print("Runs failed: %d" % len(failed_runs))
    print("Failed runs:", failed_runs)


if __name__ == "__main__":
    random_seed(20313854)
    results, failed = loop()
    print_results(results, failed)
