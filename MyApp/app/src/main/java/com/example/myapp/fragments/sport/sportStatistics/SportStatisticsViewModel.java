package com.example.myapp.fragments.sport.sportStatistics;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportStatisticsViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private HashMap<Integer, Sport> sportList;
    private HashMap<Integer, Type> typeList;

    private int userID;

    public SportStatisticsViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        typeRepository = new TypeRepository(application);
        typeSportRepository = new TypeSportRepository(application);

        userID = ((MainApplication) getApplication()).getUserID();
    }

    public HashMap<Sport, List<Pair<Type, Integer>>> processData(List<TypeSport> typeSports){

        if(typeSports.size() == 0) return new HashMap<>();
        HashMap<Sport, List<Pair<Type, Integer>>> newTypeSport = new HashMap<>();

        for(TypeSport typeSport : typeSports){
            int sportID = typeSport.getSportID();
            int typeID = typeSport.getTypeID();
            int duration = typeSport.getSportDuration();

            Sport sport = sportList.containsKey(sportID) ? sportList.get(sportID) : sportRepository.getSport(sportID).get(0);
            sportList.putIfAbsent(sportID, sport);

            Type type = typeList.containsKey(typeID) ? typeList.get(typeID) : typeRepository.getType(typeID).get(0);
            typeList.putIfAbsent(typeID, type);

            newTypeSport.putIfAbsent(sport, new ArrayList<>());
            Objects.requireNonNull(newTypeSport.get(sport)).add(new Pair<>(type, duration));
        }
        return newTypeSport;
    }

    public LiveData<List<TypeSport>> getTypeSportList() {
        return typeSportRepository.getAllTypeSport(userID);
    }

    public int getUserID() {
        return userID;
    }
}
