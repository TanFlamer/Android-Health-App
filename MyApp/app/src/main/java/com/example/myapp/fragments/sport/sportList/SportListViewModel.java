package com.example.myapp.fragments.sport.sportList;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportListViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> sportDateMerger;
    private LiveData<List<Sport>> sportLiveData;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<TypeSport>> typeSportLiveData;

    private int userID;

    public SportListViewModel(@NonNull Application application) {
        super(application);
        sportRepository = ((MainApplication) getApplication()).getSportRepository();
        typeRepository = ((MainApplication) getApplication()).getTypeRepository();
        typeSportRepository = ((MainApplication) getApplication()).getTypeSportRepository();
        initialiseLists();
        initialiseLiveDataMerger();
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void initialiseLists(){
        sportLiveData = sportRepository.getAllSport(userID);
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = typeSportRepository.getAllTypeSport(userID);
    }

    public void initialiseLiveDataMerger(){
        sportDateMerger = new MediatorLiveData<>();
        sportDateMerger.addSource(sportLiveData, sportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
    }

    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Sport> sportList, List<Type> typeList, List<TypeSport> typeSportList){
        if(sportList.size() == 0 || typeList.size() == 0 || typeSportList.size() == 0) return new HashMap<>();

        HashMap<Integer, Sport> sportHashMap = new HashMap<>();
        for(Sport sport : sportList) sportHashMap.put(sport.getSportID(), sport);

        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        HashMap<Sport, List<Pair<Type, Integer>>> newTypeSport = new HashMap<>();
        for(TypeSport typeSport : typeSportList){
            Sport sport = sportHashMap.get(typeSport.getSportID());
            Type type = typeHashMap.get(typeSport.getTypeID());
            int duration = typeSport.getSportDuration();
            newTypeSport.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(newTypeSport.get(sport)).add(new Pair<>(type, duration));
        }
        return newTypeSport;
    }

    public MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> getSportDateMerger() {
        return sportDateMerger;
    }

    public void insert(TypeSport typeSport){
        typeSportRepository.insert(typeSport);
    }

    public void update(TypeSport typeSport){
        typeSportRepository.update(typeSport);
    }

    public void delete(TypeSport typeSport){
        typeSportRepository.delete(typeSport);
    }
}
