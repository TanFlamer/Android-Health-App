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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataSportViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeSportRepository typeSportRepository;

    private HashMap<Integer, Type> typeMap;
    private List<Type> typeList;
    private List<TypeSport> typeSportList;

    private Sport sport;
    private int userID;

    public DataSportViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = this.getApplication();

        sportRepository = mainApplication.getSportRepository();
        typeSportRepository = mainApplication.getTypeSportRepository();

        typeSportList = mainApplication.getTypeSportList();
        typeList = mainApplication.getTypeList();

        typeMap = new HashMap<>();
        for(Type type : typeList)  typeMap.put(type.getTypeID(), type);

        userID = mainApplication.getUserID();
    }

    public void insertSport(long newDate){
        int sportID = (int) sportRepository.insert(new Sport(newDate, userID));
        sport = new Sport(sportID, newDate, userID);
    }

    public void deleteSport(){
        sportRepository.delete(sport);
        sport = null;
    }

    public void insertTypeSport(int typeID, int duration){
        typeSportRepository.insert(new TypeSport(sport.getSportID(), typeID, duration, userID));
    }

    public void updateTypeSport(int typeID, int duration){
        typeSportRepository.update(new TypeSport(sport.getSportID(), typeID, duration, userID));
    }

    public void deleteTypeSport(int typeID, int duration){
        typeSportRepository.delete(new TypeSport(sport.getSportID(), typeID, duration, userID));
    }

    public Pair<List<Pair<Type, Integer>>, List<Type>> populateList(long date){
        List<Sport> sports = sportRepository.findSport(userID, date);
        sport = sports.size() == 0 ? null : sports.get(0);

        if(sport == null)
            return new Pair<>(new ArrayList<>(), new ArrayList<>(typeList));
        else{
            List<Pair<Type, Integer>> newTypeSport = new ArrayList<>();
            Set<Type> typeSet = new HashSet<>(typeList);

            List<TypeSport> typeSports = new ArrayList<>(typeSportList);
            typeSports.removeIf(typeSport -> !typeSport.getSportID().equals(sport.getSportID()));

            for(TypeSport typeSport : typeSports){
                Type type = typeMap.get(typeSport.getTypeID());
                int duration = typeSport.getSportDuration();
                newTypeSport.add(new Pair<>(type, duration));
                typeSet.remove(type);
            }
            return new Pair<>(newTypeSport, new ArrayList<>(typeSet));
        }
    }

    public Sport getSport() {
        return sport;
    }
}
