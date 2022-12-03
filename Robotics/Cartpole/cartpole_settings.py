import cartpole_template as cartpole


def reward_function(env_information):
    # Reward function
    obv, reward, terminated = env_information
    termination_penalty = reward
    return reward if not terminated else termination_penalty


if __name__ == "__main__":
    # Variables
    num_tables = 5
    num_buckets = (1, 1, 6, 7)
    initial_q_table = 1
    opposite_q_learning = False

    # Discount settings
    fixed_discount_factor = False
    min_discount_factor = 0.99
    discount_steps = 100

    # Run settings
    variables = (num_tables, num_buckets, initial_q_table, opposite_q_learning)
    discount_settings = (fixed_discount_factor, min_discount_factor, discount_steps)
    run_settings = (variables, discount_settings)

    # Run cartpole
    cartpole.run_simulation(run_settings, reward_function)
