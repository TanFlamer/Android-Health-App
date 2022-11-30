package com.example.myapp.fragments.sleep.sleepList;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

import java.util.Calendar;
import java.util.Comparator;
import java.util.List;

public class SleepListViewModel extends AndroidViewModel {

    private final SleepRepository sleepRepository;
    private final LiveData<List<Sleep>> sleepList;

    public SleepListViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = (MainApplication) getApplication();
        sleepRepository = mainApplication.getSleepRepository();
        sleepList = sleepRepository.getAllSleep(mainApplication.getUserID());
    }

    public Intent sleepAdd(){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        Calendar calendar = Calendar.getInstance();
        long date = calendar.toInstant().toEpochMilli();
        intent.putExtra("date", date);
        return intent;
    }

    public Intent sleepEdit(long date){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }

    public void delete(Sleep sleep){
        sleepRepository.delete(sleep);
    }

    public Comparator<Sleep> getComparator(String data, String order){
        Comparator<Sleep> sleepComparator = Comparator.comparingInt(Sleep::getSleepID);
        switch (data) {
            case "Date Added":
                sleepComparator = Comparator.comparingInt(Sleep::getSleepID);
                break;
            case "Sleep Date":
                sleepComparator = Comparator.comparingLong(Sleep::getDate);
                break;
            case "Sleep Time":
                sleepComparator = Comparator.comparingInt(a -> normalisedTime(a.getSleepTime()));
                break;
            case "Wake Time":
                sleepComparator = Comparator.comparingInt(a -> normalisedTime(a.getWakeTime()));
                break;
            case "Sleep Duration":
                sleepComparator = Comparator.comparing(this::getDuration);
                break;
        }
        return order.equals("Ascending") ? sleepComparator : sleepComparator.reversed();
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

    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }
}
