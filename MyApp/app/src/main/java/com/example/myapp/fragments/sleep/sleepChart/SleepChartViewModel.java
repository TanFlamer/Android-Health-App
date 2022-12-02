package com.example.myapp.fragments.sleep.sleepChart;

import android.annotation.SuppressLint;
import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.formatter.DefaultValueFormatter;
import com.github.mikephil.charting.formatter.ValueFormatter;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class SleepChartViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final LiveData<List<Sleep>> sleepList;
    private final List<Sleep> currentSleepList;
    private final List<String> xAxisLabels;
    private final List<BarEntry> barEntryList;

    public SleepChartViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        SleepRepository sleepRepository = mainApplication.getSleepRepository();
        sleepList = sleepRepository.getAllSleep(mainApplication.getUserID());

        currentSleepList = new ArrayList<>();
        xAxisLabels = new ArrayList<>();
        barEntryList = new ArrayList<>();
    }

    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }

    public Pair<List<String>, List<BarEntry>> processData(List<Sleep> newSleepList, String data){
        currentSleepList.clear();
        currentSleepList.addAll(newSleepList);
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
            int yValue = returnTime(sleep, data);
            barEntryList.add(new BarEntry((float) i, (float) yValue / 60));
        }
    }

    public int returnTime(Sleep sleep, String data){
        if(data.equals("Sleep Duration"))
            return getDuration(sleep);
        else if(data.equals("Sleep Time"))
            return normalisedSleepTime(sleep);
        else
            return normalisedWakeTime(sleep);
    }

    public int normalisedSleepTime(Sleep sleep){
        int time = sleep.getSleepTime();
        return time < 720 ? time : time - 1440;
    }

    public int normalisedWakeTime(Sleep sleep){
        int time = sleep.getWakeTime();
        return time - 720;
    }

    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }

    public ValueFormatter getValueFormatter(String data){
        if(data.equals("Sleep Duration"))
            return new DefaultValueFormatter(2);
        else if(data.equals("Sleep Time"))
            return sleepTimeFormatter;
        else
            return wakeTimeFormatter;
    }

    ValueFormatter sleepTimeFormatter = new ValueFormatter() {
        @SuppressLint("DefaultLocale")
        @Override
        public String getFormattedValue(float value) {
            int time = (int) (value * 60);
            time += (time < 0) ? 1440 : 0;
            return String.format("%02d:%02d", time / 60, time % 60);
        }
    };

    ValueFormatter wakeTimeFormatter = new ValueFormatter() {
        @SuppressLint("DefaultLocale")
        @Override
        public String getFormattedValue(float value) {
            int time = (int) (value * 60) + 720;
            return String.format("%02d:%02d", time / 60, time % 60);
        }
    };
}
