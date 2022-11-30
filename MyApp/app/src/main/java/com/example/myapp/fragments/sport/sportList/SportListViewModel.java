package com.example.myapp.fragments.sport.sportList;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.sportschedule.SportScheduleRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportListViewModel extends AndroidViewModel {

    private MainApplication mainApplication;
    private final SportRepository sportRepository;
    private final TypeRepository typeRepository;
    private final SportScheduleRepository sportScheduleRepository;

    private MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> sportDataMerger;
    private LiveData<List<Sport>> sportLiveData;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<SportSchedule>> typeSportLiveData;

    private final int userID;

    public SportListViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        sportRepository = mainApplication.getSportRepository();
        typeRepository = mainApplication.getTypeRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();
        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    public void initialiseLiveData(){
        sportLiveData = sportRepository.getAllSport(userID);
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = sportScheduleRepository.getAllSportSchedule(userID);
    }

    public void initialiseLiveDataMerger(){
        sportDataMerger = new MediatorLiveData<>();
        sportDataMerger.addSource(sportLiveData, sportList -> sportDataMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDataMerger.addSource(typeLiveData, typeList -> sportDataMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDataMerger.addSource(typeSportLiveData, typeSportList -> sportDataMerger.setValue(processResults(mainApplication.getSportList(), mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
    }

    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Sport> sportList, List<Type> typeList, List<SportSchedule> sportScheduleList){
        if(sportList.size() == 0 || typeList.size() == 0 || sportScheduleList.size() == 0) return new HashMap<>();

        HashMap<Integer, Sport> sportHashMap = new HashMap<>();
        for(Sport sport : sportList) sportHashMap.put(sport.getSportID(), sport);

        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        HashMap<Sport, List<Pair<Type, Integer>>> sportScheduleHashMap = new HashMap<>();
        for(SportSchedule sportSchedule : sportScheduleList){
            Sport sport = sportHashMap.get(sportSchedule.getSportID());
            Type type = typeHashMap.get(sportSchedule.getTypeID());
            int duration = sportSchedule.getSportDuration();
            sportScheduleHashMap.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(sportScheduleHashMap.get(sport)).add(new Pair<>(type, duration));
        }
        return sportScheduleHashMap;
    }

    public MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> getSportDataMerger() {
        return sportDataMerger;
    }

    public void insert(SportSchedule sportSchedule){
        sportScheduleRepository.insert(sportSchedule);
    }

    public void update(SportSchedule sportSchedule){
        sportScheduleRepository.update(sportSchedule);
    }

    public void delete(SportSchedule sportSchedule){
        sportScheduleRepository.delete(sportSchedule);
    }

    public void deleteSport(Sport sport){
        sportRepository.delete(sport);
    }
}
