package com.example.myapp.fragments.sport.sportList;

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

public class SportListViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private LiveData<List<Sport>> sportLiveData;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<TypeSport>> typeSportLiveData;

    private MediatorLiveData<HashMap<Sport, List<Pair<Type, Integer>>>> sportDateMerger;

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
        sportDateMerger.addSource(sportLiveData, sportList -> sportDateMerger.setValue(processResults((List<Sport>) sportList, ((MainApplication) getApplication()).getTypeList(), ((MainApplication) getApplication()).getTypeSportList())));
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), (List<Type>) typeList, ((MainApplication) getApplication()).getTypeSportList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(((MainApplication) getApplication()).getSportList(), ((MainApplication) getApplication()).getTypeList(), (List<TypeSport>) typeSportList)));
    }

    public HashMap<Sport, List<Pair<Type, Integer>>> processResults(List<Sport> sportList, List<Type> typeList, List<TypeSport> typeSportList){
        System.out.println(Arrays.toString(((MainApplication) getApplication()).getSportList().toArray()));
        System.out.println(Arrays.toString(((MainApplication) getApplication()).getTypeList().toArray()));
        System.out.println(Arrays.toString(((MainApplication) getApplication()).getTypeSportList().toArray()));
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
        return  newTypeSport;
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

    /*public HashMap<Sport, List<Pair<Type, Duration>>> updateSportList(List<TypeSport> typeSports){

        if(typeSports.size() == 0) return new HashMap<>();
        HashMap<Sport, List<Pair<Type, Duration>>> newTypeSport = new HashMap<>();

        for(TypeSport typeSport : typeSports){
            int sportID = typeSport.getSportID();
            int typeID = typeSport.getTypeID();
            Duration duration = typeSport.getDuration();

            Sport sport = sportList.containsKey(sportID) ? sportList.get(sportID) : sportRepository.getSport(sportID).get(0);
            sportList.putIfAbsent(sportID, sport);

            Type type = typeList.containsKey(typeID) ? typeList.get(typeID) : typeRepository.getType(typeID).get(0);
            typeList.putIfAbsent(typeID, type);

            newTypeSport.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(newTypeSport.get(sport)).add(new Pair<>(type, duration));
        }
        return newTypeSport;
    }*/
}
