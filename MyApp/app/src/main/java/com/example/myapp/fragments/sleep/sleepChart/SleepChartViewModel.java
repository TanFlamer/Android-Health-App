package com.example.myapp.fragments.sleep.sleepChart;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.github.mikephil.charting.data.BarEntry;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class SleepChartViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private LiveData<List<Sleep>> sleepList;
    private int userID;

    private List<Sleep> currentSleepList;
    private List<String> xAxisLabels;
    private List<BarEntry> barEntryList;

    public SleepChartViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        sleepList = sleepRepository.getAllSleep(userID);

        currentSleepList = new ArrayList<>();
        xAxisLabels = new ArrayList<>();
        barEntryList = new ArrayList<>();
    }

    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }

    public int getUserID() {
        return userID;
    }

    public Pair<List<String>, List<BarEntry>> processData(List<Sleep> newSleepList, String data){
        currentSleepList = newSleepList;
        currentSleepList.sort(Comparator.comparingLong(Sleep::getDate));
        xAxisLabels.clear();
        for(Sleep sleep : currentSleepList) xAxisLabels.add(String.valueOf(Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate()));
        refreshBarEntryList(data);
        return new Pair<>(xAxisLabels, barEntryList);
    }

    public Pair<List<String>, List<BarEntry>> changeData(String data){
        refreshBarEntryList(data);
        return new Pair<>(xAxisLabels, barEntryList);
    }

    public void refreshBarEntryList(String data){
        barEntryList.clear();
        for(int i = 0; i < currentSleepList.size(); i++){
            Sleep sleep = currentSleepList.get(i);
            int yValue = data.equals("Sleep Duration") ? getDuration(sleep) : normalisedTime(sleep, data);
            barEntryList.add(new BarEntry((float) i, (float) yValue / 60));
        }
    }

    public int normalisedTime(Sleep sleep, String data){
        if(data.equals("Sleep Time")){
            int time = sleep.getSleepTime();
            return time < 720 ? time : time - 1440;
        }
        else {
            int time = sleep.getWakeTime();
            return time - 720;
        }
    }

    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }
}
