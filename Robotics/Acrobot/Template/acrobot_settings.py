import acrobot_template_quadruple as acrobot

if __name__ == "__main__":
    # Random seed
    RANDOM_SEED = 20313854

    # Run settings
    MIN_RUNS = 30

    MAX_RUNS = 50

    RUN_SETTINGS = (MIN_RUNS, MAX_RUNS)

    # Hyper parameters
    DISCOUNT_FACTOR = 0.999

    MIN_LEARNING_RATE = 0.1

    MIN_EXPLORE_RATE = 0.01

    HYPER_PARAMETERS = (DISCOUNT_FACTOR, MIN_LEARNING_RATE, MIN_EXPLORE_RATE)

    # Variables
    NUM_BUCKETS = (1, 1, 1, 1, 10, 10)

    NUM_ACTIONS = 3

    Q_TABLE_SETTINGS = (0, 0)

    TERMINATION_PENALTY = 0

    OPPOSITE_PENALTY = 0

    VARIABLES = (NUM_BUCKETS, NUM_ACTIONS, Q_TABLE_SETTINGS, TERMINATION_PENALTY, OPPOSITE_PENALTY)

    # Run cartpole
    acrobot.run_simulation(RANDOM_SEED, RUN_SETTINGS, HYPER_PARAMETERS, VARIABLES)
