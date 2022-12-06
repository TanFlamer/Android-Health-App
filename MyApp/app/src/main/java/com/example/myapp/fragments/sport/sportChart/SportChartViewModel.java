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

    public SportChartViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = getApplication();
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

    public MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> getSportDateMerger() {
        return sportDateMerger;
    }

    public int getUserID() {
        return userID;
    }

    public void initialiseLiveData(){
        sportLiveData = sportRepository.getAllSport(userID);
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = sportScheduleRepository.getAllSportSchedule(userID);
    }

    public void initialiseLiveDataMerger(){
        sportDateMerger = new MediatorLiveData<>();
        sportDateMerger.addSource(sportLiveData, sportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getSportScheduleList())));
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getSportScheduleList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getSportScheduleList())));
    }

    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Sport> sportList, List<Type> typeList, List<SportSchedule> sportScheduleList){
        if(sportList.size() == 0 || typeList.size() == 0 || sportScheduleList.size() == 0) return new HashMap<>();

        HashMap<Integer, Sport> sportHashMap = new HashMap<>();
        for(Sport sport : sportList) sportHashMap.put(sport.getSportID(), sport);

        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        HashMap<Sport, List<Pair<Type, Integer>>> newTypeSport = new HashMap<>();
        for(SportSchedule sportSchedule : sportScheduleList){
            Sport sport = sportHashMap.get(sportSchedule.getSportID());
            Type type = typeHashMap.get(sportSchedule.getTypeID());
            int duration = sportSchedule.getSportDuration();
            newTypeSport.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(newTypeSport.get(sport)).add(new Pair<>(type, duration));
        }
        return newTypeSport;
    }

    public Pair<List<String>, List<BarEntry>> processData(HashMap<Sport, List<Pair<Type, Integer>>> newSportMap, String data){
        currentSportMap.clear();
        currentSportMap.putAll(newSportMap);
        sportList.clear();
        sportList.addAll(currentSportMap.keySet());
        sportList.sort(Comparator.comparingLong(Sport::getDate));
        xAxisLabels.clear();
        for(Sport sport : sportList) xAxisLabels.add(String.valueOf(Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate()));
        refreshBarEntryList(data);
        return new Pair<>(xAxisLabels, barEntryList);
    }

    public Pair<List<String>, List<BarEntry>> changeData(String data){
        refreshBarEntryList(data);
        return new Pair<>(xAxisLabels, barEntryList);
    }

    public void refreshBarEntryList(String data){
        barEntryList.clear();
        if(data.equals("Sport Duration"))
            getTotalDuration();
        else
            getTotalCalorie();
    }

    public void getTotalDuration(){
        for(int i = 0; i < sportList.size(); i++){
            List<Pair<Type, Integer>> pairList = currentSportMap.get(sportList.get(i));
            int totalDuration = 0;
            for(Pair<Type, Integer> pair : pairList){
                totalDuration += pair.second;
            }
            barEntryList.add(new BarEntry((float) i, totalDuration));
        }
    }

    public void getTotalCalorie(){
        for(int i = 0; i < sportList.size(); i++){
            List<Pair<Type, Integer>> pairList = currentSportMap.get(sportList.get(i));
            float totalCalorie = 0;
            for(Pair<Type, Integer> pair : pairList){
                totalCalorie += pair.first.getCaloriePerMinute() * pair.second;
            }
            barEntryList.add(new BarEntry((float) i, totalCalorie));
        }
    }
}
