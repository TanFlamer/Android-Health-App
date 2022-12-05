import math
import numpy as np

ACROBOT_SAMPLE_MEAN = 293.87
ACROBOT_SAMPLE_STD = 36.64

CARTPOLE_SAMPLE_MEAN = 257.27
CARTPOLE_SAMPLE_STD = 14.94

T_VALUE = 2.3924 # 99% confidence level for 58 degrees of freedom

def get_average(time_steps):
    num_elements = len(time_steps) - 1
    if num_elements <= 100:
        average = time_steps[-1] / num_elements
    else:
        average = (time_steps[-1] - time_steps[-101]) / 100
    return average


def get_quartiles(runs, episodes):
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
        first_quartile = (episodes[midpoint] + episodes[midpoint + 1]) / 2
        third_quartile = (episodes[midpoint + runs_halved + offset] + episodes[
            midpoint + runs_halved + offset + 1]) / 2
    else:
        midpoint = (runs_halved + 1) // 2 - 1
        first_quartile = episodes[midpoint]
        third_quartile = episodes[midpoint + runs_halved + offset]

    return median, third_quartile - first_quartile

def calculate_t_value(mean, std, size, cartpole):

    if cartpole:
        sample_mean = CARTPOLE_SAMPLE_MEAN
        sample_std = CARTPOLE_SAMPLE_STD
    else:
        sample_mean = ACROBOT_SAMPLE_MEAN
        sample_std = ACROBOT_SAMPLE_STD

    mean_difference = sample_mean - mean
    sample_variance = sample_std * sample_std
    variance = std * std
    pooled_variance = ((sample_variance + variance) * (size - 1)) / (2 * size - 2)
    pooled_std = math.sqrt(pooled_variance)
    limit = mean_difference - T_VALUE * (pooled_std * math.sqrt(2 / size))
    return max(limit, 0)


def get_results(episodes, cartpole):
    runs = len(episodes)
    if runs == 0:
        return 0, 0, 0, 0, 0, 0
    elif runs == 1:
        return episodes[0], 0, episodes[0], 0, episodes[0], episodes[0]
    else:
        sample_sum = sum(episodes)
        sample_variance = (sum(np.square(episodes)) - (sample_sum * sample_sum) / runs) / (runs - 1)
        mean = sample_sum / runs
        standard_deviation = math.sqrt(sample_variance)

    t_value = calculate_t_value(mean, standard_deviation, runs, cartpole)
    median, inter_quartile_range = get_quartiles(runs, episodes)
    return mean, standard_deviation, median, inter_quartile_range, max(episodes), min(episodes), t_value


def print_results(result, failed_runs):
    print("")
    print("Mean = %.2f" % result[0])
    print("Standard Deviation = %.2f" % result[1])
    print("Median = %.1f" % result[2])
    print("Inter-Quartile Range = %.1f" % result[3])
    print("Max = %d" % result[4])
    print("Min = %d" % result[5])
    print("Runs failed: %d" % len(failed_runs))
    print("Failed runs:", failed_runs)
    print("Limit = %.2f" % result[6])
