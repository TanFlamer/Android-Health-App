import math
import numpy as np
import scipy


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

def calculate_t_value(t_test_values, second_sample_results):
    confidence_level, first_sample_mean, first_sample_std, first_sample_size = t_test_values
    second_sample_mean, second_sample_std, second_sample_size = second_sample_results

    mean_difference = first_sample_mean - second_sample_mean
    first_sample_variance = first_sample_std * first_sample_std
    second_sample_variance = second_sample_std * second_sample_std

    first_sample_data = first_sample_variance * (first_sample_size - 1)
    second_sample_data = second_sample_variance * (second_sample_size - 1)
    degrees_of_freedom = first_sample_size + second_sample_size - 2

    pooled_variance = (first_sample_data + second_sample_data) / degrees_of_freedom
    pooled_std = math.sqrt(pooled_variance)
    critical_value = scipy.stats.t.ppf(confidence_level, degrees_of_freedom)

    t_test_bottom = pooled_std * math.sqrt(1 / first_sample_size + 1 / second_sample_size)
    difference = mean_difference - critical_value * t_test_bottom
    return max(difference, 0)


def get_results(episodes, t_test_values):
    runs = len(episodes)
    if runs == 0:
        return (0, 0, 0), (0, 0, 0, 0), 0
    elif runs == 1:
        return (episodes[0], 0, 1), (episodes[0], 0, episodes[0], episodes[0]), 0
    else:
        sample_sum = sum(episodes)
        sample_variance = (sum(np.square(episodes)) - (sample_sum * sample_sum) / runs) / (runs - 1)

        mean = sample_sum / runs
        standard_deviation = math.sqrt(sample_variance)
        median, inter_quartile_range = get_quartiles(runs, episodes)

        main_results = (mean, standard_deviation, runs)
        supporting_results = (median, inter_quartile_range, max(episodes), min(episodes))

        t_value = calculate_t_value(t_test_values, main_results)
        return main_results, supporting_results, t_value


def print_results(results, failed):
    main_results, supporting_results, t_value = results
    print("")
    print("Mean = %.2f" % main_results[0])
    print("Standard Deviation = %.2f" % main_results[1])
    print("Median = %.1f" % supporting_results[0])
    print("Inter-Quartile Range = %.1f" % supporting_results[1])
    print("Max = %d" % supporting_results[2])
    print("Min = %d" % supporting_results[3])
    print("Runs failed: %d" % len(failed))
    print("Failed runs:", failed)
    print("Difference = %.2f" % t_value)
