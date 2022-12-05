import math
import cartpole_template as cartpole

def base_reward(reward):
    return reward

def termination_penalty(reward, obv, terminated):
    if terminated:
        return -reward
    else:
        _, future_terminated = future_position(obv)
        return reward if not future_terminated else -reward

def time_reward(time):
    return math.exp(time / 100)

def uniform_reward(obv):
    angle, _ = future_position(obv)
    reward = 10 * max(math.pi / 15 - abs(angle), 0)
    return reward

def exponential_reward(obv):
    angle, _ = future_position(obv)
    score = max(math.pi / 15 - abs(angle), 0)
    reward = 10 * (math.exp(score) - 1)
    return reward

def logarithmic_reward(obv):
    angle, _ = future_position(obv)
    score = max(math.pi / 15 - abs(angle), 0)
    reward = 10 * (math.log(1 + score))
    return reward

def future_position(obv):
    _, _, angle, velocity = obv
    threshold = math.pi / 15
    new_angle = angle + 0.02 * velocity
    terminated = new_angle < -threshold or new_angle > threshold
    return new_angle, terminated

def reward_function(env_information):
    # Reward function
    obv, reward, terminated, time = env_information
    return base_reward(reward)


if __name__ == "__main__":
    # Variables
    num_tables = 1
    num_buckets = (1, 1, 6, 3)
    initial_q_table = 0
    opposite_q_learning = False

    # Discount settings
    fixed_discount_factor = True
    min_discount_factor = 0.99
    discount_steps = 100

    # Run settings
    variables = (num_tables, num_buckets, initial_q_table, opposite_q_learning)
    discount_settings = (fixed_discount_factor, min_discount_factor, discount_steps)
    run_settings = (variables, discount_settings)

    # Run cartpole
    cartpole.run_simulation(run_settings, reward_function)
