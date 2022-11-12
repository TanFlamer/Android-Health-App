# Adapted from: https://medium.com/@tuzzer/cart-pole-balancing-with-q-learning-b54c6068d947

import results_processing
import math
import random
import gym
import numpy as np

# Initialize the "Cart-Pole" environment
env = gym.make('CartPole-v0')

# Number of discrete states (bucket) per state dimension
NUM_BUCKETS = (1, 1, 6, 7)  # (x, x', theta, theta')

# Number of discrete actions
NUM_ACTIONS = env.action_space.n  # (left, right)

# Bounds for each discrete state
STATE_BOUNDS = list(zip(env.observation_space.low, env.observation_space.high))
STATE_BOUNDS[1] = (-0.5, 0.5)
STATE_BOUNDS[3] = (-math.radians(50), math.radians(50))

# Learning related constants
MIN_EXPLORE_RATE = 0.01
MIN_LEARNING_RATE = 0.1
DISCOUNT_FACTOR = 0.999

# Defining the simulation related constants
NUM_TRAIN_EPISODES = 500
MAX_TRAIN_T = 200
MAX_RUNS = 50


def loop(runs):
    episodes = []
    failed_runs = []
    run_number = 0
    while len(episodes) < runs and run_number < MAX_RUNS:
        run_number += 1
        episode, solved = train(run_number)
        if solved:
            episodes.append(episode)
        else:
            failed_runs.append(run_number)
            print("Run %2d failed in %d episodes - *" % (run_number, NUM_TRAIN_EPISODES))
    return results_processing.get_results(episodes), failed_runs


def train(run):
    # Instantiating the learning related parameters
    learning_rate = get_learning_rate(0)
    explore_rate = get_explore_rate(0)

    q_table_a = np.zeros(NUM_BUCKETS + (NUM_ACTIONS,))
    q_table_b = np.zeros(NUM_BUCKETS + (NUM_ACTIONS,))
    q_table_c = np.zeros(NUM_BUCKETS + (NUM_ACTIONS,))

    # q_table_a = np.random.randn(*NUM_BUCKETS, NUM_ACTIONS) * 1
    # q_table_b = np.random.randn(*NUM_BUCKETS, NUM_ACTIONS) * 1
    # q_table_c = np.random.randn(*NUM_BUCKETS, NUM_ACTIONS) * 1

    q_tables = [q_table_a, q_table_b, q_table_c]

    time_steps = [0]
    episodes_to_solve = 0
    solved = False

    for episode in range(1, NUM_TRAIN_EPISODES + 1):

        # Reset the environment
        obv, _ = env.reset()

        # the initial state
        state_0 = state_to_bucket(obv)

        # Discount factor (Grey out unused line)
        # discount_factor = min(0.75 + 0.005 * episode, 0.999)  # 0.75, 0.005
        discount_factor = DISCOUNT_FACTOR

        # Opposition penalty (Default 0)
        opposite_penalty = 0

        for t in range(1, MAX_TRAIN_T + 1):
            env.render()

            # Save old state
            old_obv = obv

            # Select an action
            action = select_action(state_0, explore_rate, q_table_a + q_table_b + q_table_c)
            opposite_action = 1 - action

            # Get Q table indexes
            index_main = random.randint(0, 2)
            index_secondary = random.randint(0, 1)
            index_secondary += 1 if index_secondary >= index_main else 0

            q_table_main = q_tables[index_main]
            q_table_secondary = q_tables[index_secondary]

            # Execute the action
            obv, reward, terminated, _, _ = env.step(action)
            opposite_obv, opposite_reward, opposite_terminated, _, _ = step(old_obv, opposite_action)

            # Observe the result
            state = state_to_bucket(obv)
            opposite_state = state_to_bucket(opposite_obv)

            # Get best Q value
            best_q = q_table_secondary[state + (np.argmax(q_table_main[state]),)]
            opposite_best_q = q_table_secondary[opposite_state + (np.argmax(q_table_main[opposite_state]),)]

            if terminated:
                q_table_main[state_0 + (action,)] += learning_rate * (
                        reward + discount_factor * best_q - q_table_main[state_0 + (action,)])
                q_table_main[state_0 + (opposite_action,)] += -opposite_penalty

                # Opposition Q-Learning (Grey out if unused)
                q_table_main[state_0 + (opposite_action,)] += learning_rate * (
                        opposite_reward + discount_factor * opposite_best_q - q_table_main[state_0 + (opposite_action,)])
                time_steps.append(t + time_steps[-1])
                break

            # Update the Q based on the result
            q_table_main[state_0 + (action,)] += learning_rate * (
                    reward + discount_factor * best_q - q_table_main[state_0 + (action,)])
            q_table_main[state_0 + (opposite_action,)] += -opposite_penalty

            # Opposition Q-Learning (Grey out if unused)
            q_table_main[state_0 + (opposite_action,)] += learning_rate * (
                    opposite_reward + discount_factor * opposite_best_q - q_table_main[state_0 + (opposite_action,)])

            # Setting up for the next iteration
            state_0 = state

        else:
            time_steps.append(MAX_TRAIN_T + time_steps[-1])

        # It's considered done when average for last 100 time steps is >= 195.0
        if results_processing.get_average(time_steps) >= 195.0:
            print("Run %2d solved in %d episodes" % (run, episode))
            episodes_to_solve = episode
            solved = True
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
        action = np.argmax(q_table[state])
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


def step(state, action):
    gravity = 9.8
    masscart = 1.0
    masspole = 0.1
    total_mass = masspole + masscart
    length = 0.5  # actually half the pole's length
    polemass_length = masspole * length
    force_mag = 10.0
    tau = 0.02  # seconds between state updates
    kinematics_integrator = "euler"

    # Angle at which to fail the episode
    theta_threshold_radians = 12 * 2 * math.pi / 360
    x_threshold = 2.4

    x, x_dot, theta, theta_dot = state
    force = force_mag if action == 1 else -force_mag
    costheta = math.cos(theta)
    sintheta = math.sin(theta)

    temp = (force + polemass_length * theta_dot ** 2 * sintheta) / total_mass
    thetaacc = (gravity * sintheta - costheta * temp) / (length * (4.0 / 3.0 - masspole * costheta ** 2 / total_mass))
    xacc = temp - polemass_length * thetaacc * costheta / total_mass

    if kinematics_integrator == "euler":
        x = x + tau * x_dot
        x_dot = x_dot + tau * xacc
        theta = theta + tau * theta_dot
        theta_dot = theta_dot + tau * thetaacc
    else:  # semi-implicit euler
        x_dot = x_dot + tau * xacc
        x = x + tau * x_dot
        theta_dot = theta_dot + tau * thetaacc
        theta = theta + tau * theta_dot

    # New state
    state = (x, x_dot, theta, theta_dot)

    # Reward
    reward = 1.0

    # Terminated
    terminated = bool(
        x < -x_threshold
        or x > x_threshold
        or theta < -theta_threshold_radians
        or theta > theta_threshold_radians
    )

    return np.array(state, dtype=np.float32), reward, terminated, False, {}


def random_seed(seed):
    np.random.seed(seed)
    random.seed(seed)
    env.action_space.seed(seed)
    env.reset(seed=seed)


if __name__ == "__main__":
    random_seed(20313854)
    results, failed = loop(30)
    results_processing.print_results(results, failed)
