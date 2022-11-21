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
import com.github.mikephil.charting.data.BarEntry;

import java.util.ArrayList;
import java.util.List;

public class SportChartViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private LiveData<List<Sport>> sportList;
    private LiveData<List<Type>> typeList;
    private LiveData<List<TypeSport>> typeSportList;

    private int userID;

    public SportChartViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        typeRepository = new TypeRepository(application);
        typeSportRepository = new TypeSportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        sportList = sportRepository.getAllSport(userID);
        typeList = typeRepository.getAllTypes(userID);
        typeSportList = typeSportRepository.getAllTypeSport(userID);
    }

    public LiveData<List<Sport>> getSportList(){
        return sportList;
    }

    public int getUserID() {
        return userID;
    }

    public List<BarEntry> processData(List<Sport> sportList){
        return new ArrayList<>();
    }
}
