package com.example.myapp.fragments.sport.sportStatistics;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportStatisticsViewModel extends AndroidViewModel {

    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private MediatorLiveData<HashMap<Type, int[]>> sportDateMerger;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<TypeSport>> typeSportLiveData;

    private int userID;

    public SportStatisticsViewModel(@NonNull Application application) {
        super(application);
        typeRepository = ((MainApplication) getApplication()).getTypeRepository();
        typeSportRepository = ((MainApplication) getApplication()).getTypeSportRepository();
        userID = ((MainApplication) getApplication()).getUserID();
        initialiseLists();
        initialiseLiveDataMerger();
    }

    public void initialiseLists(){
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = typeSportRepository.getAllTypeSport(userID);
    }

    public void initialiseLiveDataMerger(){
        sportDateMerger = new MediatorLiveData<>();
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
    }

    public HashMap<Type, int[]> processResults(List<Type> typeList, List<TypeSport> typeSportList){
        if(typeList.size() == 0 || typeSportList.size() == 0) return new HashMap<>();

        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        HashMap<Type, int[]> sportResults = new HashMap<>();
        for(TypeSport typeSport : typeSportList){
            Type type = typeHashMap.get(typeSport.getTypeID());
            int duration = typeSport.getSportDuration();
            sportResults.putIfAbsent(type, new int[] {0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0});
            int[] results = sportResults.get(type);
            results[0] += duration; //total duration
            results[1] = Math.max(duration, results[1]); //max duration
            results[2] = Math.min(duration, results[2]); //min duration
            results[3] += 1; //number of days
            sportResults.put(type, results);
        }
        return sportResults;
    }

    public MediatorLiveData<HashMap<Type, int[]>> getSportDateMerger() {
        return sportDateMerger;
    }

    public int getUserID() {
        return userID;
    }
}
