import cartpole_base


def get_termination_equation(q_table, state_0, action):
    # get reward or penalty on termination (+0 for no change)
    penalised_score = q_table[state_0 + (action,)]
    return penalised_score


def get_update_equations(q_table, state_0, action, learning_rate, reward, best_q):
    # get error and opposite action
    error = learning_rate * (reward + DISCOUNT_FACTOR * best_q - q_table[state_0 + (action,)])
    opposite_action = NUM_ACTIONS - action - 1

    # update action and opposite action score (+0 for no change)
    action_score = q_table[state_0 + (action,)] + error
    opposite_action_score = q_table[state_0 + (opposite_action,)]

    return action_score, opposite_action_score, opposite_action


if __name__ == "__main__":
    # granularity (4-tuple for cartpole, 6-tuple for acrobot)
    NUM_BUCKETS = (1, 1, 6, 3)

    # actions (2 for cartpole, 3 for acrobot)
    NUM_ACTIONS = 2

    # random_seed
    RANDOM_SEED = 20313854

    # discount_factor
    DISCOUNT_FACTOR = 0.995

    # q_table_settings (table_type, factor, bias, min_val, max_val)
    # q_table_settings[0] == 0 : zeros (1 argument)
    # q_table_settings[0] == 1 : gaussian (3 arguments)
    # q_table_settings[0] == 2 : uniform (5 arguments)
    q_table_settings = (0,)

    cartpole_base.run_simulation(NUM_BUCKETS, NUM_ACTIONS, RANDOM_SEED, q_table_settings,
                                 get_termination_equation, get_update_equations)
