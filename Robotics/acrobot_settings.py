import math
import acrobot_template as acrobot

def base_reward(reward):
    return reward

def time_penalty(time, reward, terminated):
    penalty = -math.exp(time / 200)
    return reward if terminated else penalty

def velocity_reward(obv, reward, terminated):
    _, _, _, _, velocity1, velocity2 = obv
    penalty = abs(velocity1 + velocity2) - 13 * math.pi
    return reward if terminated else penalty

# Termination = -cos(a) - cos(a + b) > 1.0
#  cos(a + b) = cos(a) * cos(b) - sin(a) * sin(b)
# -cos(a) - cos(a + b) = sin(a) * sin(b) - cos(a) * (cos(b) + 1)
def height_reward(obv, reward, terminated):
    cosA, sinA, cosB, sinB, _, _ = obv
    penalty = sinA * sinB - cosA * (cosB + 1) - 2
    return reward if terminated else penalty

def reward_function(env_information):
    # Reward function
    obv, reward, terminated, time = env_information
    return base_reward(reward)


if __name__ == "__main__":
    # T-test
    confidence_level = 0.99
    sample_mean = 293.87
    sample_std = 36.64
    sample_size = 30

    # Variables
    num_tables = 2
    num_buckets = (1, 1, 1, 1, 10, 10)
    num_actions = 2
    initial_q_table = 0
    opposite_q_learning = False

    # Discount settings
    fixed_discount_factor = True
    min_discount_factor = 0.99
    discount_steps = 100

    # Run settings
    variables = (num_tables, num_buckets, num_actions, initial_q_table, opposite_q_learning)
    discount_settings = (fixed_discount_factor, min_discount_factor, discount_steps)
    run_settings = (variables, discount_settings)
    t_test_values = (confidence_level, sample_mean, sample_std, sample_size)

    # Run acrobot
    acrobot.run_simulation(run_settings, reward_function, t_test_values)
