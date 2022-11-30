package com.example.myapp.subActivities.sport;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.databasefiles.sportschedule.SportScheduleRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SportDataViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private SportScheduleRepository sportScheduleRepository;

    private HashMap<Integer, Type> typeMap;
    private List<Type> typeList;
    private List<SportSchedule> sportScheduleList;

    private Sport sport;
    private int userID;

    public SportDataViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = this.getApplication();

        sportRepository = mainApplication.getSportRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();

        sportScheduleList = mainApplication.getSportScheduleList();
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
        sportScheduleRepository.insert(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    public void updateTypeSport(int typeID, int duration){
        sportScheduleRepository.update(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    public void deleteTypeSport(int typeID, int duration){
        sportScheduleRepository.delete(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    public Pair<List<Pair<Type, Integer>>, List<Type>> populateList(long date){
        sport = sportRepository.findSport(userID, date);
        if(sport == null)
            return new Pair<>(new ArrayList<>(), new ArrayList<>(typeList));
        else{
            List<Pair<Type, Integer>> newTypeSport = new ArrayList<>();
            Set<Type> typeSet = new HashSet<>(typeList);

            List<SportSchedule> sportSchedules = new ArrayList<>(sportScheduleList);
            sportSchedules.removeIf(typeSport -> !typeSport.getSportID().equals(sport.getSportID()));

            for(SportSchedule sportSchedule : sportSchedules){
                Type type = typeMap.get(sportSchedule.getTypeID());
                int duration = sportSchedule.getSportDuration();
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
