package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.entity.TypeSport;
import com.example.myapp.databaseFiles.repository.SportRepository;
import com.example.myapp.databaseFiles.repository.TypeRepository;
import com.example.myapp.databaseFiles.repository.TypeSportRepository;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class DataSportViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;
    private MutableLiveData<List<TypeSport>> typeSportList;
    private LocalDate date;
    private int userID;

    private HashMap<Integer, Sport> sportList;
    private HashMap<Integer, Type> typeList;

    public DataSportViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        typeRepository = new TypeRepository(application);
        typeSportRepository = new TypeSportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();

        typeSportList = new MutableLiveData<>();
        typeSportList.setValue(new ArrayList<>());

        sportList = new HashMap<>();
        typeList = new HashMap<>();
    }

    public LocalDate getDate() {
        return date;
    }

    public List<Sport> findSport(LocalDate date){
        return sportRepository.findSport(userID, date);
    }

    public void updateTypeSportList(List<TypeSport> newTypeSportList){
        typeSportList.setValue(newTypeSportList);
    }

    public LiveData<List<TypeSport>> getTypeSportList() {
        return typeSportList;
    }

    public List<Pair<Type, Duration>> processData(List<TypeSport> typeSportList) {
        if (typeSportList.size() == 0) return new ArrayList<>();
        List<Pair<Type, Duration>> newTypeSport = new ArrayList<>();

        for (TypeSport typeSport : typeSportList) {
            int sportID = typeSport.getSportID();
            int typeID = typeSport.getTypeID();
            Duration duration = typeSport.getDuration();

            Sport sport = sportList.containsKey(sportID) ? sportList.get(sportID) : sportRepository.getSport(sportID).get(0);
            sportList.putIfAbsent(sportID, sport);

            Type type = typeList.containsKey(typeID) ? typeList.get(typeID) : typeRepository.getType(typeID).get(0);
            typeList.putIfAbsent(typeID, type);

            newTypeSport.add(new Pair<>(type, duration));
        }
        return newTypeSport;
    }
}
