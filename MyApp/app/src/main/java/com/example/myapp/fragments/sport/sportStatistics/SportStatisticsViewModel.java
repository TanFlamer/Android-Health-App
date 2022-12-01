package com.example.myapp.fragments.sport.sportStatistics;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.type.TypeRepository;
import com.example.myapp.databasefiles.sportschedule.SportScheduleRepository;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportStatisticsViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final TypeRepository typeRepository;
    private final SportScheduleRepository sportScheduleRepository;

    private MediatorLiveData<HashMap<Type, double[]>> sportDateMerger;
    private LiveData<List<Type>> typeLiveData;
    private LiveData<List<SportSchedule>> typeSportLiveData;

    private final int userID;

    public SportStatisticsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        typeRepository = mainApplication.getTypeRepository();
        sportScheduleRepository = mainApplication.getSportScheduleRepository();
        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    public void initialiseLiveData(){
        typeLiveData = typeRepository.getAllTypes(userID);
        typeSportLiveData = sportScheduleRepository.getAllSportSchedule(userID);
    }

    public void initialiseLiveDataMerger(){
        sportDateMerger = new MediatorLiveData<>();
        sportDateMerger.addSource(typeLiveData, typeList -> sportDateMerger.setValue(processResults(mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
        sportDateMerger.addSource(typeSportLiveData, typeSportList -> sportDateMerger.setValue(processResults(mainApplication.getTypeList(), mainApplication.getSportScheduleList())));
    }

    public HashMap<Type, double[]> processResults(List<Type> typeList, List<SportSchedule> sportScheduleList){
        if(typeList.size() == 0 || sportScheduleList.size() == 0) return new HashMap<>();

        HashMap<Integer, Type> typeHashMap = new HashMap<>();
        for(Type type : typeList) typeHashMap.put(type.getTypeID(), type);

        HashMap<Type, double[]> sportResults = new HashMap<>();
        for(SportSchedule sportSchedule : sportScheduleList){
            Type type = typeHashMap.get(sportSchedule.getTypeID());
            int duration = sportSchedule.getSportDuration();
            sportResults.putIfAbsent(type, new double[] {0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0});
            double[] results = sportResults.get(type);
            results[0] += duration; //total duration
            results[1] = Math.max(duration, results[1]); //max duration
            results[2] = Math.min(duration, results[2]); //min duration
            results[3] += 1; //number of days
            sportResults.put(type, results);
        }
        return compileResults(sportResults);
    }

    public HashMap<Type, double[]> compileResults(HashMap<Type, double[]> results){
        for(Type type : results.keySet()){
            double[] initial = results.get(type);
            double[] processed = new double[9];
            double calorie = type.getCaloriePerMinute();
            processed[0] = initial[0];
            processed[1] = initial[0] * calorie;
            processed[2] = initial[3];
            processed[3] = initial[0] / initial[3];
            processed[4] = processed[3] * calorie;
            processed[5] = initial[1];
            processed[6] = initial[2];
            processed[7] = initial[1] * calorie;
            processed[8] = initial[2] * calorie;
            results.put(type, processed);
        }
        return results;
    }

    public void sortSportStatistics(List<Type> typeList, HashMap<Type, double[]> sportResults, String data, String order){
        Comparator<Type> typeComparator = getComparator(data, order, sportResults);
        typeList.sort(typeComparator);
    }

    public Comparator<Type> getComparator(String data, String order, HashMap<Type, double[]> sportResults){
        Comparator<Type> typeComparator = Comparator.comparingInt(Type::getTypeID);
        switch (data) {
            case "Date Added":
                typeComparator = Comparator.comparingInt(Type::getTypeID);
                break;
            case "Name":
                typeComparator = Comparator.comparing(Type::getTypeName);
                break;
            case "Total Duration":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 0, sportResults));
                break;
            case "Total Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 1, sportResults));
                break;
            case "Total Days":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 2, sportResults));
                break;
            case "Average Duration":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 3, sportResults));
                break;
            case "Average Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 4, sportResults));
                break;
            case "Max Duration":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 5, sportResults));
                break;
            case "Min Duration":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 6, sportResults));
                break;
            case "Max Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 7, sportResults));
                break;
            case "Min Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 8, sportResults));
                break;
        }
        return order.equals("Ascending") ? typeComparator : typeComparator.reversed();
    }

    public double getResults(Type type, int result, HashMap<Type, double[]> sportResults){
        return Objects.requireNonNull(sportResults.get(type))[result];
    }

    public MediatorLiveData<HashMap<Type, double[]>> getSportDateMerger() {
        return sportDateMerger;
    }

    public int getUserID() {
        return userID;
    }
}
