package com.example.myapp.fragments.sport.sportList;

import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.sportschedule.SportSchedule;
import com.example.myapp.databaseFiles.sportschedule.SportScheduleRepository;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.subActivities.sport.SportDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportListViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SportRepository sportRepository;
    private final TypeRepository typeRepository;
    private final SportScheduleRepository sportScheduleRepository;

    private MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> sportDataMerger;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<SportSchedule>> typeSportLiveData;

    private final int userID;

    //constructor for sport list view model
    public SportListViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        sportRepository = mainApplication.getSportRepository();
        typeRepository = mainApplication.getTypeRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();
        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    //initialise live data of sport type and sport schedule list
    public void initialiseLiveData(){
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = sportScheduleRepository.getAllSportSchedule(userID);
    }

    //merge live data of sport type and sport schedule list
    public void initialiseLiveDataMerger(){
        sportDataMerger = new MediatorLiveData<>();
        sportDataMerger.addSource(typeLiveData, typeList -> sportDataMerger.setValue(processResults(mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDataMerger.addSource(typeSportLiveData, typeSportList -> sportDataMerger.setValue(processResults(mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
    }

    //link sport types to sport data
    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Type> typeList, List<SportSchedule> sportScheduleList){
        List<Sport> sportList = mainApplication.getSportList();
        //if sport data list, sport type list or sport catalogue list empty, return empty map
        if(sportList.size() == 0 || typeList.size() == 0 || sportScheduleList.size() == 0) return new HashMap<>();

        //get all sport data
        HashMap<Integer, Sport> sportHashMap = new HashMap<>();
        for(Sport sport : sportList) sportHashMap.put(sport.getSportID(), sport);

        //get all sport types
        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        //link sport types to sport data
        HashMap<Sport, List<Pair<Type, Integer>>> sportScheduleHashMap = new HashMap<>();
        for(SportSchedule sportSchedule : sportScheduleList){
            Sport sport = sportHashMap.get(sportSchedule.getSportID());
            Type type = typeHashMap.get(sportSchedule.getTypeID());
            int duration = sportSchedule.getSportDuration();
            sportScheduleHashMap.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(sportScheduleHashMap.get(sport)).add(new Pair<>(type, duration));
        }
        //return hashmap of sport data and sport type list
        return sportScheduleHashMap;
    }

    //send current date to add sport data activity
    public Intent sportAdd(){
        Intent intent = new Intent(getApplication(), SportDataActivity.class);
        long date = LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
        intent.putExtra("date", date);
        return intent;
    }

    //send selected date to edit sport data activity
    public Intent sportEdit(long date){
        Intent intent = new Intent(getApplication(), SportDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }

    //dialog to validate sport data deletion
    public AlertDialog deleteSportList(Context context, Sport sport){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> {
                    sportRepository.delete(sport);
                    LocalDate date = Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
                    updateSaveLogs("Sport data for " + date + " deleted");
                })
                .setNegativeButton("No", null)
                .create();
    }

    //sort sport data and sport type lists
    public void sortSportLists(List<Sport> sportList, HashMap<Sport, List<Pair<Type, Integer>>> newTypeSports, String data, String order){
        Comparator<Sport> sportComparator = getSportComparator(data, order, newTypeSports);
        //remove null values from sport list
        sportList.removeIf(Objects::isNull);
        sportList.sort(sportComparator);
        Comparator<Pair<Type, Integer>> typeComparator = getTypeComparator(data, order);
        for(List<Pair<Type, Integer>> typeList : newTypeSports.values()) {
            //remove null values from sport type list
            typeList.removeIf(Objects::isNull);
            typeList.sort(typeComparator);
        }
    }

    //get comparator to sort sport data list
    public Comparator<Sport> getSportComparator(String data, String order, HashMap<Sport, List<Pair<Type, Integer>>> typeSports){
        Comparator<Sport> sportComparator = Comparator.comparingLong(Sport::getDate);
        switch (data) {
            case "Sport Date":
                sportComparator = Comparator.comparingLong(Sport::getDate);
                break;
            case "Calories":
                sportComparator = Comparator.comparingDouble(sport -> getCalories(sport, typeSports));
                break;
            case "Duration":
                sportComparator = Comparator.comparingDouble(sport -> getDuration(sport, typeSports));
                break;
        }
        return order.equals("Ascending") ? sportComparator : sportComparator.reversed();
    }

    //get comparator to sort sport type list
    public Comparator<Pair<Type, Integer>> getTypeComparator(String data, String order){
        Comparator<Pair<Type, Integer>> typeComparator = Comparator.comparing(a -> a.first.getTypeName());
        switch (data) {
            case "Name":
                typeComparator = Comparator.comparing(a -> a.first.getTypeName());
                break;
            case "Calories":
                typeComparator = Comparator.comparingDouble(a -> a.first.getCaloriePerMinute() * a.second);
                break;
            case "Duration":
                typeComparator = Comparator.comparingInt(a -> a.second);
                break;
        }
        return order.equals("Ascending") ? typeComparator : typeComparator.reversed();
    }

    //get total calories of sport types in sport data
    public double getCalories(Sport sport, HashMap<Sport, List<Pair<Type, Integer>>> typeSports){
        double calories = 0;
        for(Pair<Type, Integer> pair : Objects.requireNonNull(typeSports.get(sport)))
            calories += pair.first.getCaloriePerMinute() * pair.second;
        return calories;
    }

    //get total duration of sport types in sport data
    public int getDuration(Sport sport, HashMap<Sport, List<Pair<Type, Integer>>> typeSports){
        int duration = 0;
        for(Pair<Type, Integer> pair : Objects.requireNonNull(typeSports.get(sport)))
            duration += pair.second;
        return duration;
    }

    //return live data merger of sport type and sport schedule list
    public MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> getSportDataMerger() {
        return sportDataMerger;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
