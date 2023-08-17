package com.example.myapp.fragments.sleep.sleepChart;

import android.annotation.SuppressLint;
import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepRepository;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.formatter.DefaultValueFormatter;
import com.github.mikephil.charting.formatter.ValueFormatter;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class SleepChartViewModel extends AndroidViewModel {

    private final LiveData<List<Sleep>> sleepList;
    private final List<Sleep> currentSleepList;
    private final List<String> xAxisLabels;
    private final List<BarEntry> barEntryList;

    //constructor for sleep chart view model
    public SleepChartViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = (MainApplication) getApplication();
        SleepRepository sleepRepository = mainApplication.getSleepRepository();
        sleepList = sleepRepository.getAllSleep(mainApplication.getUserID());

        currentSleepList = new ArrayList<>();
        xAxisLabels = new ArrayList<>();
        barEntryList = new ArrayList<>();
    }

    //return live data of sleep data list
    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }

    //convert sleep data list to date labels and bar entries
    public Pair<List<String>, List<BarEntry>> processData(List<Sleep> newSleepList, String data){
        //clear old sleep data list
        currentSleepList.clear();
        //add new sleep data list
        currentSleepList.addAll(newSleepList);
        //sort sleep data list according to date in ascending order
        currentSleepList.sort(Comparator.comparingLong(Sleep::getDate));
        //clear old date labels
        xAxisLabels.clear();
        //add new date labels
        for(Sleep sleep : currentSleepList) xAxisLabels.add(String.valueOf(Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate()));
        //refresh bar entries list
        refreshBarEntryList(data);
        //return date labels list and bar entries list
        return new Pair<>(xAxisLabels, barEntryList);
    }

    //change bar data
    public Pair<List<String>, List<BarEntry>> changeData(String data){
        //refresh bar entry list
        refreshBarEntryList(data);
        //return date labels list and bar entries list
        return new Pair<>(xAxisLabels, barEntryList);
    }

    //refresh bar entry list
    public void refreshBarEntryList(String data){
        //clear old bar entries list
        barEntryList.clear();
        //add new bar entries list
        for(int i = 0; i < currentSleepList.size(); i++){
            Sleep sleep = currentSleepList.get(i);
            //return data depending on spinner data selected
            int yValue = returnTime(sleep, data);
            //add new bar entry
            barEntryList.add(new BarEntry((float) i, (float) yValue / 60));
        }
    }

    //return data depending on spinner data selected
    public int returnTime(Sleep sleep, String data){
        if(data.equals("Sleep Duration"))
            return getDuration(sleep); //return sleep duration
        else if(data.equals("Sleep Time"))
            return normalisedSleepTime(sleep); //return normalised sleep time
        else
            return normalisedWakeTime(sleep); //return normalised wake time
    }

    //return normalised sleep time
    public int normalisedSleepTime(Sleep sleep){
        int time = sleep.getSleepTime();
        return time < 720 ? time : time - 1440;
    }

    //return normalised wake time
    public int normalisedWakeTime(Sleep sleep){
        int time = sleep.getWakeTime();
        return time - 720;
    }

    //return sleep duration
    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }

    //return value formatter for y-axis
    public ValueFormatter getValueFormatter(String data){
        if(data.equals("Sleep Duration"))
            return new DefaultValueFormatter(2);
        else if(data.equals("Sleep Time"))
            return sleepTimeFormatter;
        else
            return wakeTimeFormatter;
    }

    //sleep time value formatter
    ValueFormatter sleepTimeFormatter = new ValueFormatter() {
        @SuppressLint("DefaultLocale")
        @Override
        public String getFormattedValue(float value) {
            int time = (int) (value * 60);
            time += (time < 0) ? 1440 : 0;
            return String.format("%02d:%02d", time / 60, time % 60);
        }
    };

    //wake time value formatter
    ValueFormatter wakeTimeFormatter = new ValueFormatter() {
        @SuppressLint("DefaultLocale")
        @Override
        public String getFormattedValue(float value) {
            int time = (int) (value * 60) + 720;
            return String.format("%02d:%02d", time / 60, time % 60);
        }
    };
}
