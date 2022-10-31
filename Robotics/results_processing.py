import math
import numpy as np


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


def get_results(episodes):
    runs = len(episodes)
    if runs == 0:
        return 0, 0, 0, 0, 0, 0
    elif runs == 1:
        return episodes[0], 0, episodes[0], 0, episodes[0], episodes[0]
    else:
        mean = sum(episodes) / runs
        variance = (sum(np.square(episodes)) / runs) - (mean * mean)
        standard_deviation = math.sqrt(variance)

    median, inter_quartile_range = get_quartiles(runs, episodes)
    return mean, standard_deviation, median, inter_quartile_range, max(episodes), min(episodes)


def print_results(result, failed_runs):
    print("")
    print("Mean = %f" % result[0])
    print("Standard Deviation = %f" % result[1])
    print("Median = %.1f" % result[2])
    print("Inter-Quartile Range = %.1f" % result[3])
    print("Max = %d" % result[4])
    print("Min = %d" % result[5])
    print("Runs failed: %d" % len(failed_runs))
    print("Failed runs:", failed_runs)
