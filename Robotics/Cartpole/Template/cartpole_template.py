# Adapted from: https://medium.com/@tuzzer/cart-pole-balancing-with-q-learning-b54c6068d947

import results_processing
import math
import random
import gym
import numpy as np

# Initialize the "Cart-Pole" environment
env = gym.make('CartPole-v0')

# Random seed
RANDOM_SEED = 20313854

# Number of discrete actions
NUM_ACTIONS = env.action_space.n  # (left, right)

# Bounds for each discrete state
STATE_BOUNDS = list(zip(env.observation_space.low, env.observation_space.high))
STATE_BOUNDS[1] = (-0.5, 0.5)
STATE_BOUNDS[3] = (-math.radians(50), math.radians(50))

# Defining the simulation related constants
NUM_TRAIN_EPISODES = 500
MAX_TRAIN_T = 200

# Run settings
MIN_RUNS = 30
MAX_RUNS = 50

# Hyper parameters
MAX_DISCOUNT_FACTOR = 0.999
MIN_LEARNING_RATE = 0.1
MIN_EXPLORE_RATE = 0.01


def loop(run_settings, reward_function):
    episodes = []
    failed_runs = []
    run_number = 0
    while len(episodes) < MIN_RUNS and run_number < MAX_RUNS:
        run_number += 1
        episode, solved = train(run_number, run_settings, reward_function)
        if solved:
            episodes.append(episode)
        else:
            failed_runs.append(run_number)
            print("Run %2d failed in %d episodes - *" % (run_number, NUM_TRAIN_EPISODES))
    return results_processing.get_results(episodes), failed_runs


def train(run, run_settings, reward_function):
    # Unpacking run settings
    variables, discount_settings = run_settings

    # Unpacking variables
    num_tables, num_buckets, initial_q_table, opposite_penalty, opposite_q_learning = variables

    # Unpacking discount settings
    fixed_discount_factor, min_discount_factor, discount_steps = discount_settings

    # Discount step size
    discount_step_size = (1 - min_discount_factor) / discount_steps

    # Instantiating the learning related parameters
    learning_rate = get_learning_rate(0)
    explore_rate = get_explore_rate(0)

    # Q-tables list
    q_tables = []

    for x in range(num_tables):
        q_tables.append(np.zeros(num_buckets + (
            NUM_ACTIONS,)) if initial_q_table == 0 else np.random.randn(*num_buckets, NUM_ACTIONS) * initial_q_table)

    time_steps = [0]
    episodes_to_solve = 0
    solved = False

    for episode in range(1, NUM_TRAIN_EPISODES + 1):

        # Reset the environment
        obv, _ = env.reset()

        # the initial state
        state_0 = state_to_bucket(obv, num_buckets)

        # Get discount factor
        discount_factor = MAX_DISCOUNT_FACTOR if fixed_discount_factor else min(
            min_discount_factor + discount_step_size * episode, MAX_DISCOUNT_FACTOR)

        # Get parameters
        parameters = (learning_rate, discount_factor, opposite_penalty)

        for t in range(1, MAX_TRAIN_T + 1):
            env.render()

            # Old observation for opposite action
            old_obv = obv

            # Select an action
            action = select_action(state_0, explore_rate, q_tables)
            opposite_action = 1 - action

            # Get Q table indexes
            index_main = 0
            index_secondary = 0
            q_table_length = len(q_tables)

            if q_table_length > 1:
                index_main = random.randint(0, q_table_length - 1)
                index_secondary = random.randint(0, q_table_length - 2)
                index_secondary += 1 if index_secondary >= index_main else 0

            q_table_main = q_tables[index_main]
            q_table_secondary = q_tables[index_secondary]

            # Execute the action
            obv, reward, terminated, _, _ = env.step(action)

            # Observe the result
            state = state_to_bucket(obv, num_buckets)
            opposite_state = None

            # Get reward function
            reward = reward_function((obv, reward, terminated))
            opposite_reward = None

            # Get best Q value
            best_q = q_table_secondary[state + (np.argmax(q_table_main[state]),)]
            opposite_best_q = None

            if opposite_q_learning:
                # Execute opposite action
                opposite_obv, opposite_reward, opposite_terminated, _, _ = step(old_obv, opposite_action)

                # Observe the result
                opposite_state = state_to_bucket(opposite_obv, num_buckets)

                # Get reward function
                opposite_reward = reward_function((opposite_obv, opposite_reward, opposite_terminated))

                # Get best Q value
                opposite_best_q = q_table_secondary[opposite_state + (np.argmax(q_table_main[opposite_state]),)]

            state_information = (state_0, state, action, reward, best_q)
            opposite_state_information = (opposite_state, opposite_reward, opposite_best_q)

            # Termination
            if terminated:
                update_q_table(q_table_main, parameters, state_information,
                               opposite_state_information if opposite_q_learning else None)
                time_steps.append(t + time_steps[-1])
                break

            # Update the Q based on the result
            update_q_table(q_table_main, parameters, state_information,
                           opposite_state_information if opposite_q_learning else None)

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
        learning_rate = get_learning_rate(episode)
        explore_rate = get_explore_rate(episode)

    # print(q_table)
    return episodes_to_solve, solved


def update_q_table(q_table, parameters, state_information, opposite_state_information):
    # Unpacking variables
    learning_rate, discount_factor, opposite_penalty = parameters
    state_0, state, action, reward, best_q = state_information
    opposite_action = 1 - action

    # Updating Q tables
    q_table[state_0 + (action,)] += learning_rate * (
            reward + discount_factor * best_q - q_table[state_0 + (action,)])

    q_table[state_0 + (opposite_action,)] += -opposite_penalty

    if opposite_state_information is not None:
        opposite_state, opposite_reward, opposite_best_q = opposite_state_information
        q_table[state_0 + (opposite_action,)] += learning_rate * (
                opposite_reward + discount_factor * opposite_best_q - q_table[state_0 + (opposite_action,)])


def select_action(state, explore_rate, q_tables):
    # Select a random action
    if random.random() < explore_rate:
        action = env.action_space.sample()
    # Select the action with the highest q
    else:
        action = np.argmax(sum(q_tables)[state])
    return action


def get_explore_rate(t):
    return max(MIN_EXPLORE_RATE, min(1.0, 1.0 - math.log10((t + 1) / 25)))


def get_learning_rate(t):
    return max(MIN_LEARNING_RATE, min(0.5, 1.0 - math.log10((t + 1) / 25)))


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


def run_simulation(run_settings, reward_function):
    random_seed(RANDOM_SEED)
    results, failed = loop(run_settings, reward_function)
    results_processing.print_results(results, failed)
