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

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class SportDataViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SportRepository sportRepository;
    private final SportScheduleRepository sportScheduleRepository;

    private final HashMap<Integer, Type> typeMap;
    private final List<Type> typeList;
    private final List<SportSchedule> sportScheduleList;

    private final int userID;
    private Sport sport;

    //constructor for view model
    public SportDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();

        sportRepository = mainApplication.getSportRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();

        sportScheduleList = mainApplication.getSportScheduleList();
        typeList = mainApplication.getTypeList();
        userID = mainApplication.getUserID();

        typeMap = new HashMap<>();
        for(Type type : typeList)  typeMap.put(type.getTypeID(), type);
    }

    //insert new sport data to database
    public void insertSport(long newDate){
        LocalDate localDate = Instant.ofEpochMilli(newDate).atZone(ZoneId.systemDefault()).toLocalDate();
        updateSaveLogs("Sleep data for " + localDate + " added");
        int sportID = (int) sportRepository.insert(new Sport(newDate, userID));
        sport = new Sport(sportID, newDate, userID);
    }

    //insert new sport schedule to database
    public void insertTypeSport(int typeID, int duration){
        String typeName = Objects.requireNonNull(typeMap.get(typeID)).getTypeName();
        updateSaveLogs("Sport Type " + typeName + " for " + duration + " minutes added to " + getDate());
        sportScheduleRepository.insert(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    //update existing sport schedule in database
    public void updateTypeSport(int typeID, int duration){
        String typeName = Objects.requireNonNull(typeMap.get(typeID)).getTypeName();
        updateSaveLogs("Sport Type " + typeName + " from " + getDate() + " updated to " + duration + " minutes");
        sportScheduleRepository.update(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    //delete existing sport schedule from database
    public void deleteTypeSport(int typeID, int duration){
        String typeName = Objects.requireNonNull(typeMap.get(typeID)).getTypeName();
        updateSaveLogs("Sport Type " + typeName + " deleted from " + getDate());
        sportScheduleRepository.delete(new SportSchedule(sport.getSportID(), typeID, duration, userID));
    }

    //convert long to date
    public LocalDate getDate(){
        return Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
    }

    //initialise selected and unselected sport type list
    public Pair<List<Pair<Type, Integer>>, List<Type>> populateList(long date){
        sport = sportRepository.findSport(userID, date);
        if(sport == null) //if no sport data given, set all sport type in unselected list
            return new Pair<>(new ArrayList<>(), new ArrayList<>(typeList));
        else{
            List<Pair<Type, Integer>> newTypeSport = new ArrayList<>();
            Set<Type> typeSet = new HashSet<>(typeList);

            //else if sport data given, get all sport schedule belonging to sport data
            List<SportSchedule> sportSchedules = new ArrayList<>(sportScheduleList);
            sportSchedules.removeIf(typeSport -> !typeSport.getSportID().equals(sport.getSportID()));

            //populate selected sport type list with sport schedule list
            for(SportSchedule sportSchedule : sportSchedules){
                Type type = typeMap.get(sportSchedule.getTypeID());
                int duration = sportSchedule.getSportDuration();
                newTypeSport.add(new Pair<>(type, duration));
                typeSet.remove(type);
            }
            return new Pair<>(newTypeSport, new ArrayList<>(typeSet));
        }
    }

    //return sport data
    public Sport getSport() {
        return sport;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
