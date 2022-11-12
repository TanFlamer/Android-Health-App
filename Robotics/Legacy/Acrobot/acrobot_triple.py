# Adapted from: https://medium.com/@tuzzer/cart-pole-balancing-with-q-learning-b54c6068d947
from gym.envs.classic_control.acrobot import wrap, bound, rk4
from numpy import cos, pi, sin

import results_processing
import math
import random
import gym
import numpy as np

# Initialize the "Cart-Pole" environment
env = gym.make('Acrobot-v1')

# Number of discrete states (bucket) per state dimension
NUM_BUCKETS = (1, 1, 1, 1, 10, 10)

# Number of discrete actions
NUM_ACTIONS = 3

# Bounds for each discrete state
STATE_BOUNDS = list(zip(env.observation_space.low, env.observation_space.high))

# Learning related constants
MIN_EXPLORE_RATE = 0.01
MIN_LEARNING_RATE = 0.1
DISCOUNT_FACTOR = 0.999

# Defining the simulation related constants
NUM_TRAIN_EPISODES = 1000
MAX_TRAIN_T = 500
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

    # Initial Q-table (Grey out unused)
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

        for t in range(1, MAX_TRAIN_T + 1):
            env.render()

            # Save old state
            old_obv = env.state

            # Select an action
            action = select_action(state_0, explore_rate, q_table_a + q_table_b + q_table_c)
            opposite_action = NUM_ACTIONS - action - 1

            # Get Q table indexes
            index_main = random.randint(0, 2)
            index_secondary = random.randint(0, 1)
            index_secondary += 1 if index_secondary >= index_main else 0

            q_table_main = q_tables[index_main]
            q_table_secondary = q_tables[index_secondary]

            # Execute the action
            obv, reward, terminated, _, _ = env.step(action if NUM_ACTIONS == 3 else action * 2)
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

                # Opposition Q-Learning (Grey out if unused)
                if action != opposite_action:
                    q_table_main[state_0 + (opposite_action,)] += learning_rate * (
                            opposite_reward + discount_factor * opposite_best_q - q_table_main[state_0 + (opposite_action,)])
                time_steps.append(t + time_steps[-1])
                break

            # Update the Q based on the result
            q_table_main[state_0 + (action,)] += learning_rate * (
                    reward + discount_factor * best_q - q_table_main[state_0 + (action,)])

            # Opposition Q-Learning (Grey out if unused)
            if action != opposite_action:
                q_table_main[state_0 + (opposite_action,)] += learning_rate * (
                        opposite_reward + discount_factor * opposite_best_q - q_table_main[state_0 + (opposite_action,)])

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
        explore_rate = get_explore_rate(episode)
        learning_rate = get_learning_rate(episode)

    # print(q_table)
    return episodes_to_solve, solved


def select_action(state, explore_rate, q_table):
    # Select a random action
    if random.random() < explore_rate:
        action = env.action_space.sample() if NUM_ACTIONS == 3 else random.randint(0, 1)
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
    dt = 0.2

    MAX_VEL_1 = 4 * pi
    MAX_VEL_2 = 9 * pi

    AVAIL_TORQUE = [-1.0, 0.0, +1]
    torque = AVAIL_TORQUE[action]

    # Now, augment the state with our force action, so it can be passed to _dsdt
    s_augmented = np.append(state, torque)
    ns = rk4(_dsdt, s_augmented, [0, dt])

    ns[0] = wrap(ns[0], -pi, pi)
    ns[1] = wrap(ns[1], -pi, pi)
    ns[2] = bound(ns[2], -MAX_VEL_1, MAX_VEL_1)
    ns[3] = bound(ns[3], -MAX_VEL_2, MAX_VEL_2)

    state = ns
    terminated = bool(-cos(state[0]) - cos(state[1] + state[0]) > 1.0)
    reward = -1.0 if not terminated else 0.0

    return np.array([cos(state[0]), sin(state[0]), cos(state[1]), sin(state[1]), state[2], state[3]], dtype=np.float32
                    ), reward, terminated, False, {}


def _dsdt(s_augmented):
    book_or_nips = "book"
    m1 = 1.0  #: [kg] mass of link 1
    m2 = 1.0  #: [kg] mass of link 2
    l1 = 1.0  # [m]
    lc1 = 0.5  #: [m] position of the center of mass of link 1
    lc2 = 0.5  #: [m] position of the center of mass of link 2
    I1 = 1.0  #: moments of inertia for both links
    I2 = 1.0  #: moments of inertia for both links
    g = 9.8

    a = s_augmented[-1]
    s = s_augmented[:-1]

    theta1 = s[0]
    theta2 = s[1]
    dtheta1 = s[2]
    dtheta2 = s[3]

    d1 = (m1 * lc1 ** 2 + m2 * (l1 ** 2 + lc2 ** 2 + 2 * l1 * lc2 * cos(theta2)) + I1 + I2)
    d2 = m2 * (lc2 ** 2 + l1 * lc2 * cos(theta2)) + I2
    phi2 = m2 * lc2 * g * cos(theta1 + theta2 - pi / 2.0)
    phi1 = (-m2 * l1 * lc2 * dtheta2 ** 2 * sin(theta2) - 2 * m2 * l1 * lc2 * dtheta2 * dtheta1 * sin(theta2)
            + (m1 * lc1 + m2 * l1) * g * cos(theta1 - pi / 2) + phi2)

    if book_or_nips == "nips":
        # the following line is consistent with the description in the paper
        ddtheta2 = (a + d2 / d1 * phi1 - phi2) / (m2 * lc2 ** 2 + I2 - d2 ** 2 / d1)
    else:
        # the following line is consistent with the java implementation and the book
        ddtheta2 = (a + d2 / d1 * phi1 - m2 * l1 * lc2 * dtheta1 ** 2 * sin(theta2) - phi2) / (
                m2 * lc2 ** 2 + I2 - d2 ** 2 / d1)

    ddtheta1 = -(d2 * ddtheta2 + phi1) / d1

    return dtheta1, dtheta2, ddtheta1, ddtheta2, 0.0


def random_seed(seed):
    np.random.seed(seed)
    random.seed(seed)
    env.action_space.seed(seed)
    env.reset(seed=seed)


if __name__ == "__main__":
    random_seed(20313854)
    results, failed = loop(30)
    results_processing.print_results(results, failed)
