package com.example.myapp.fragments.sleep.sleepStatistics;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.Transformations;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;

import java.util.List;

public class SleepStatisticsViewModel extends AndroidViewModel {

    private final LiveData<List<Sleep>> sleepList;

    //constructor for sleep statistics view model
    public SleepStatisticsViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = (MainApplication) getApplication();
        SleepRepository sleepRepository = mainApplication.getSleepRepository();
        int userID = mainApplication.getUserID();
        sleepList = sleepRepository.getAllSleep(userID);
    }

    //compile sleep statistics
    public double[] processResults(List<Sleep> sleepList){
        //if no sleep data, return empty statistics
        if(sleepList.size() == 0) return new double[9];

        int[] results = new int[] {0, 0, 1440, 719, 720, 1439, 0, 0};
        for(Sleep sleep : sleepList){
            int duration = sleep.getWakeTime() - sleep.getSleepTime();
            duration += (duration >= 0) ? 0 : 1440;
            results[0] += duration; //total time
            results[1] = Math.max(duration, results[1]); //longest sleep
            results[2] = Math.min(duration, results[2]); //shortest sleep
            results[3] = (normalisedSleepTime(sleep.getSleepTime()) < normalisedSleepTime(results[3])) ? sleep.getSleepTime() : results[3]; //earliest sleep
            results[4] = (normalisedSleepTime(sleep.getSleepTime()) > normalisedSleepTime(results[4])) ? sleep.getSleepTime() : results[4]; //latest sleep
            results[5] = (normalisedWakeTime(sleep.getWakeTime()) < normalisedWakeTime(results[5])) ? sleep.getWakeTime() : results[5]; //earliest wake
            results[6] = (normalisedWakeTime(sleep.getWakeTime()) > normalisedWakeTime(results[6])) ? sleep.getWakeTime() : results[6]; //latest wake
            results[7] += 1; //sleep days
        }
        //convert sleep results
        return compileResults(results);
    }

    //convert sleep results
    public double[] compileResults(int[] results){
        double[] newResults = new double[9];
        newResults[0] = (double) results[0] / 60;
        newResults[1] = (double) results[7];
        newResults[2] = (double) results[1] / 60;
        newResults[3] = (double) results[2] / 60;
        newResults[4] = (double) results[0] / (results[7] * 60);
        newResults[5] = results[3];
        newResults[6] = results[4];
        newResults[7] = results[5];
        newResults[8] = results[6];
        return newResults;
    }

    //return normalised sleep time
    public int normalisedSleepTime(int time){
        time -= 720;
        if(time < 0) time += 1440;
        return time;
    }

    //return normalised wake time
    public int normalisedWakeTime(int time){
        return time - 720;
    }

    //return live data for sleep data list
    public LiveData<double[]> getSleepLiveData(){
        return Transformations.map(sleepList, this::processResults);
    }
}
