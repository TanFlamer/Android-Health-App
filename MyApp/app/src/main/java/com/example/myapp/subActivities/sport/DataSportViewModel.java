package com.example.myapp.subActivities.sport;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataSportViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;
    public List<Type> typeList;

    private int userID;
    private int sportID;
    private long date;

    public DataSportViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = this.getApplication();
        sportRepository = new SportRepository(application);
        typeRepository = mainApplication.getTypeRepository();
        typeSportRepository = mainApplication.getTypeSportRepository();
        typeList = mainApplication.getTypeList();
        userID = mainApplication.getUserID();
    }

    public void insertSport(long newDate){
        sportID = (int) sportRepository.insert(new Sport(date, userID));
        date = newDate;
    }

    public void deleteSport(){
        sportRepository.delete(new Sport(sportID, date, userID));
        sportID = 0;
        date = 0;
    }

    public void insertTypeSport(int typeID, int duration){
        typeSportRepository.insert(new TypeSport(sportID, typeID, duration, userID));
    }

    public void updateTypeSport(int typeID, int duration){
        typeSportRepository.update(new TypeSport(sportID, typeID, duration, userID));
    }

    public void deleteTypeSport(int typeID, int duration){
        typeSportRepository.delete(new TypeSport(sportID, typeID, duration, userID));
    }

    public Pair<List<Pair<Type, Integer>>, List<Type>> populateList(long date){
        List<Sport> sports = sportRepository.findSport(userID, date);
        if(sports.size() == 0) {
            return new Pair<>(new ArrayList<>(), new ArrayList<>(typeList));
        }
        else{
            List<Pair<Type, Integer>> newTypeSport = new ArrayList<>();
            Set<Type> typeSet = new HashSet<>(typeList);
            for(TypeSport typeSport : typeSportRepository.getTypeSport(sports.get(0).getSportID())){
                Type type = typeRepository.getType(typeSport.getTypeID()).get(0);
                int duration = typeSport.getSportDuration();
                newTypeSport.add(new Pair<>(type, duration));
                typeSet.remove(type);
            }
            return new Pair<>(newTypeSport, new ArrayList<>(typeSet));
        }
    }

    public int getSportID() {
        return sportID;
    }
}
