package com.example.myapp.fragments.sleep.sleepChart;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepRepository;
import com.github.mikephil.charting.data.BarEntry;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class SleepChartViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private LiveData<List<Sleep>> sleepList;
    private int userID;

    public SleepChartViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        sleepList = sleepRepository.getAllSleep(userID);
    }

    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }

    public int getUserID() {
        return userID;
    }

    public Pair<List<String>, List<BarEntry>> processData(List<Sleep> sleepList){
        List<String> xAxisLabels = new ArrayList<>();
        List<BarEntry> barEntryList = new ArrayList<>();
        sleepList.sort(Comparator.comparingLong(Sleep::getDate));
        for(int i = 0; i < sleepList.size(); i++){
            LocalDate date = Instant.ofEpochMilli(sleepList.get(i).getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
            xAxisLabels.add(date.toString());
            System.out.println(date);
            barEntryList.add(new BarEntry((float) i, (float) getDuration(sleepList.get(i)) / 60));
        }
        return new Pair<>(xAxisLabels, barEntryList);
    }

    public int normalisedTime(int time){
        time -= 720;
        if(time < 0) time += 1440;
        return time;
    }

    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }
}
