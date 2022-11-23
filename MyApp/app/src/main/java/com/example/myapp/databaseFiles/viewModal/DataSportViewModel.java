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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataSportViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    public List<Type> typeList;
    private final int userID;

    public DataSportViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = this.getApplication();
        sportRepository = new SportRepository(application);
        typeRepository = mainApplication.getTypeRepository();
        typeSportRepository = mainApplication.getTypeSportRepository();

        typeList = mainApplication.getTypeList();
        userID = mainApplication.getUserID();
    }

    public Pair<List<Pair<Type, Duration>>, List<Type>> populateList(LocalDate date){
        List<Sport> sports = sportRepository.findSport(userID, date);
        if(sports.size() == 0) {
            return new Pair<>(new ArrayList<>(), new ArrayList<>(typeList));
        }
        else{
            List<Pair<Type, Duration>> newTypeSport = new ArrayList<>();
            Set<Type> typeSet = new HashSet<>(typeList);
            for(TypeSport typeSport : typeSportRepository.getTypeSport(sports.get(0).getSportID())){
                Type type = typeRepository.getType(typeSport.getTypeID()).get(0);
                Duration duration = typeSport.getDuration();
                newTypeSport.add(new Pair<>(type, duration));
                typeSet.remove(type);
            }
            return new Pair<>(newTypeSport, new ArrayList<>(typeSet));
        }
    }
}
