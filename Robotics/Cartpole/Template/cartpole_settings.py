import cartpole_template as cartpole


def reward_function(env_information):
    obv, reward, terminated = env_information
    # termination_penalty = 1  # -1.945
    return reward


if __name__ == "__main__":
    # Variables
    num_tables = 1

    num_buckets = (1, 1, 6, 7)  # (1, 1, 6, 3)

    initial_q_table = 0

    opposite_penalty = 0  # -0.15

    opposite_q_learning = False

    variables = (num_tables, num_buckets, initial_q_table, opposite_penalty, opposite_q_learning)

    # Discount settings
    fixed_discount_factor = True

    min_discount_factor = 0.75

    discount_steps = 50

    discount_settings = (fixed_discount_factor, min_discount_factor, discount_steps)

    # Run settings
    run_settings = (variables, discount_settings)

    # Run cartpole
    cartpole.run_simulation(run_settings, reward_function)
