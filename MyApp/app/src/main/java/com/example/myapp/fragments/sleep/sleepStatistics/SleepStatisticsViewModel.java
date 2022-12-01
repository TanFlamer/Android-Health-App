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

    private SleepRepository sleepRepository;
    private LiveData<List<Sleep>> sleepList;
    private int userID;

    public SleepStatisticsViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        sleepList = sleepRepository.getAllSleep(userID);
    }

    public double[] processResults(List<Sleep> sleepList){
        if(sleepList.size() == 0) return new double[9];

        int[] results = new int[] {0, 0, 1440, 1440, 0, 1440, 0, 0};
        for(Sleep sleep : sleepList){
            int duration = sleep.getWakeTime() - sleep.getSleepTime();
            duration += (duration >= 0) ? 0 : 1440;
            results[0] += duration; //total time
            results[1] = Math.max(duration, results[1]); //longest sleep
            results[2] = Math.min(duration, results[2]); //shortest sleep
            results[3] = (normalised(sleep.getSleepTime()) < normalised(results[3])) ? sleep.getSleepTime() : results[3]; //earliest sleep
            results[4] = (normalised(sleep.getSleepTime()) > normalised(results[4])) ? sleep.getSleepTime() : results[4]; //latest sleep
            results[5] = (normalised(sleep.getWakeTime()) < normalised(results[5])) ? sleep.getWakeTime() : results[5]; //earliest wake
            results[6] = (normalised(sleep.getWakeTime()) > normalised(results[6])) ? sleep.getWakeTime() : results[6]; //latest wake
            results[7] += 1; //sleep days
        }
        return new double[] { results[0], results[7], results[1], results[2],
                (double) results[0] / results[7], results[3], results[4], results[5], results[6] };
    }

    public int normalised(int time){
        time -= 720;
        if(time < 0) time += 1440;
        return time;
    }

    public LiveData<double[]> getSleepLiveData(){
        return Transformations.map(sleepList, this::processResults);
    }

    public int getUserID() {
        return userID;
    }
}
