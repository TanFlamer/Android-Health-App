package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.entity.TypeSport;
import com.example.myapp.databaseFiles.repository.SportRepository;
import com.example.myapp.databaseFiles.repository.TypeRepository;
import com.example.myapp.databaseFiles.repository.TypeSportRepository;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListData;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListItem;

import java.time.Duration;
import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SportListViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;
    private LiveData<List<TypeSport>> typeSportList;
    private int userID;

    public SportListViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        typeRepository = new TypeRepository(application);
        typeSportRepository = new TypeSportRepository(application);
        userID = loadUserID();
        typeSportList = typeSportRepository.getAllTypeSport(userID);
    }

    public int loadUserID(){
        MainApplication appState = this.getApplication();
        return appState.getUserID();
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

    public List<TypeSport> findTypeSport(int sportID, int typeID){
        return typeSportRepository.findTypeSport(sportID, typeID);
    }

    public List<SportExpandableListItem> updateSportList(){
        if(typeSportList.getValue() == null) return new ArrayList<>();
        HashMap<Integer, List<Integer>> sportMap = new HashMap<>();
        HashMap<Integer, Duration> timeMap = new HashMap<>();
        for(TypeSport typeSport : typeSportList.getValue()){
            int sportID = typeSport.getSportID();
            int typeID = typeSport.getTypeID();
            sportMap.putIfAbsent(sportID, new ArrayList<>());
            sportMap.get(sportID).add(typeID);
            timeMap.put(typeID, typeSport.getDuration());
        }
        return convertParentData(sportMap, timeMap);
    }

    public List<SportExpandableListItem> convertParentData(HashMap<Integer, List<Integer>> sportMap, HashMap<Integer, Duration> timeMap){
        List<SportExpandableListItem> parentList = new ArrayList<>();
        sportMap.forEach((sportID, typeIDList) -> {
            Sport sport = sportRepository.getSport(sportID).get(0);
            List<SportExpandableListData> childList = convertChildData(typeIDList, timeMap);
            parentList.add(new SportExpandableListItem(sport, childList));
        });
        return parentList;
    }

    public List<SportExpandableListData> convertChildData(List<Integer> typeIDList, HashMap<Integer, Duration> timeMap){
        List<SportExpandableListData> childList = new ArrayList<>();
        for(Integer typeID : typeIDList){
            Type type = typeRepository.getType(typeID).get(0);
            Duration duration = timeMap.get(typeID);
            childList.add(new SportExpandableListData(type, duration));
        }
        return childList;
    }

    public LiveData<List<TypeSport>> getTypeSportList() {
        return typeSportList;
    }
}
