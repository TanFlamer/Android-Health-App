package com.example.myapp.fragments.sleep.sleepList;

import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Comparator;
import java.util.List;

public class SleepListViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SleepRepository sleepRepository;
    private final LiveData<List<Sleep>> sleepList;

    //constructor for sleep list view model
    public SleepListViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        sleepRepository = mainApplication.getSleepRepository();
        sleepList = sleepRepository.getAllSleep(mainApplication.getUserID());
    }

    //add or update current date sleep data
    public Intent sleepAdd(){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        long date = LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
        intent.putExtra("date", date);
        return intent;
    }

    //update selected date sleep data
    public Intent sleepEdit(long date){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }

    //dialog to validate sleep data deletion
    public AlertDialog deleteDialog(Context context, Sleep sleep){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> {
                    sleepRepository.delete(sleep);
                    LocalDate date = Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
                    updateSaveLogs("Sleep data for " + date + " deleted");
                })
                .setNegativeButton("No", null)
                .create();
    }

    //sort sleep list
    public void sortSleepList(List<Sleep> sleepList, String data, String order){
        Comparator<Sleep> sleepComparator = getComparator(data, order);
        sleepList.sort(sleepComparator);
    }

    //get comparator to sort sleep list
    public Comparator<Sleep> getComparator(String data, String order){
        Comparator<Sleep> sleepComparator = Comparator.comparingLong(Sleep::getDate);
        switch (data) {
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

    //return normalised sleep and wake time
    public int normalisedTime(int time){
        time -= 720;
        if(time < 0) time += 1440;
        return time;
    }

    //return sleep duration
    public int getDuration(Sleep sleep){
        int duration = sleep.getWakeTime() - sleep.getSleepTime();
        return (duration >= 0) ? duration : duration + 1440;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }

    //return live data of sleep data list
    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }
}
