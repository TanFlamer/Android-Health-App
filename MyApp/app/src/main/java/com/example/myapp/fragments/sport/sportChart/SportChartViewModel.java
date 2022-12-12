package com.example.myapp.fragments.sport.sportChart;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.sportschedule.SportScheduleRepository;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;
import com.github.mikephil.charting.data.BarEntry;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportChartViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SportRepository sportRepository;
    private final TypeRepository typeRepository;
    private final SportScheduleRepository sportScheduleRepository;

    private MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> sportDateMerger;
    private LiveData<List<Sport>> sportLiveData;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<SportSchedule>> typeSportLiveData;

    private final HashMap<Sport, List<Pair<Type, Integer>>> currentSportMap;
    private final List<Sport> sportList;
    private final List<String> xAxisLabels;
    private final List<BarEntry> barEntryList;

    private final int userID;

    //constructor for sport chart view model
    public SportChartViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        sportRepository = mainApplication.getSportRepository();
        typeRepository = mainApplication.getTypeRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();

        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();

        currentSportMap = new HashMap<>();
        sportList = new ArrayList<>();
        xAxisLabels = new ArrayList<>();
        barEntryList = new ArrayList<>();
    }

    //return live data merger of sport data
    public MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> getSportDateMerger() {
        return sportDateMerger;
    }

    //initialise live data of sport data list, sport type list and sport catalogue list
    public void initialiseLiveData(){
        //initialise live data of sport data list
        sportLiveData = sportRepository.getAllSport(userID);
        //initialise live data of sport type list
        typeLiveData = typeRepository.getAllTypes(userID);
        //initialise live data of sport catalogue list
        typeSportLiveData = sportScheduleRepository.getAllSportSchedule(userID);
    }

    //merge live data of sport data list, sport type list and sport catalogue list
    public void initialiseLiveDataMerger(){
        sportDateMerger = new MediatorLiveData<>();
        sportDateMerger.addSource(sportLiveData, sportList -> sportDateMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
    }

    //link sport types to sport data
    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Sport> sportList, List<Type> typeList, List<SportSchedule> sportScheduleList){
        //if sport data list, sport type list and sport catalogue list, return empty hash map
        if(sportList.size() == 0 || typeList.size() == 0 || sportScheduleList.size() == 0) return new HashMap<>();

        //get all sport data
        HashMap<Integer, Sport> sportHashMap = new HashMap<>();
        for(Sport sport : sportList) sportHashMap.put(sport.getSportID(), sport);

        //get all sport types
        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        //link sport types to sport data
        HashMap<Sport, List<Pair<Type, Integer>>> newTypeSport = new HashMap<>();
        for(SportSchedule sportSchedule : sportScheduleList){
            Sport sport = sportHashMap.get(sportSchedule.getSportID());
            Type type = typeHashMap.get(sportSchedule.getTypeID());
            int duration = sportSchedule.getSportDuration();
            newTypeSport.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(newTypeSport.get(sport)).add(new Pair<>(type, duration));
        }
        //return hashmap of sport data, sport types and sport duration
        return newTypeSport;
    }

    //convert sport data list, sport type list and sport catalogue list to date labels and bar entries
    public Pair<List<String>, List<BarEntry>> processData(HashMap<Sport, List<Pair<Type, Integer>>> newSportMap, String data){
        //clear old sport data map
        currentSportMap.clear();
        //add new sport data map
        currentSportMap.putAll(newSportMap);
        //clear old sport data list
        sportList.clear();
        //add new sport data list
        sportList.addAll(currentSportMap.keySet());
        //remove null values from sport list
        sportList.removeIf(Objects::isNull);
        //sort sport data list according to date in ascending order
        sportList.sort(Comparator.comparingLong(Sport::getDate));
        //clear old date labels
        xAxisLabels.clear();
        //add new date labels
        for(Sport sport : sportList) xAxisLabels.add(String.valueOf(Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate()));
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
        if(data.equals("Sport Duration"))
            getTotalDuration(); //return total sport duration bar entries list
        else
            getTotalCalorie(); //return total sport calorie bar entries list
    }

    //return total sport duration bar entries list
    public void getTotalDuration(){
        for(int i = 0; i < sportList.size(); i++){
            List<Pair<Type, Integer>> pairList = currentSportMap.get(sportList.get(i));
            int totalDuration = 0;
            for(Pair<Type, Integer> pair : pairList){
                totalDuration += pair.second;
            }
            //add new bar entry
            barEntryList.add(new BarEntry((float) i, totalDuration));
        }
    }

    //return total sport calorie bar entries list
    public void getTotalCalorie(){
        for(int i = 0; i < sportList.size(); i++){
            List<Pair<Type, Integer>> pairList = currentSportMap.get(sportList.get(i));
            float totalCalorie = 0;
            for(Pair<Type, Integer> pair : pairList){
                totalCalorie += pair.first.getCaloriePerMinute() * pair.second;
            }
            //add new bar entry
            barEntryList.add(new BarEntry((float) i, totalCalorie));
        }
    }
}
