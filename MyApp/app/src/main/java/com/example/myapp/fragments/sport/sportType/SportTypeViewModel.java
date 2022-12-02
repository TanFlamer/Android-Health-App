package com.example.myapp.fragments.sport.sportType;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;

import java.util.Comparator;
import java.util.List;

public class SportTypeViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final TypeRepository typeRepository;
    private final LiveData<List<Type>> typeList;
    private final int userID;

    public SportTypeViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        typeRepository = mainApplication.getTypeRepository();
        userID = mainApplication.getUserID();
        typeList = typeRepository.getAllTypes(userID);
    }

    public void delete(Type type){
        typeRepository.delete(type);
        updateSaveLogs("Sport type " + type.getTypeName() + " deleted");
    }

    public LiveData<List<Type>> getTypeList(){
        return typeList;
    }

    public int getUserID() {
        return userID;
    }

    public void sortTypeList(List<Type> typeList, String data, String order){
        Comparator<Type> typeComparator = getComparator(data, order);
        typeList.sort(typeComparator);
    }

    public Comparator<Type> getComparator(String data, String order){
        Comparator<Type> typeComparator = Comparator.comparingInt(Type::getTypeID);
        switch (data) {
            case "Date Added":
                typeComparator = Comparator.comparingInt(Type::getTypeID);
                break;
            case "Name":
                typeComparator = Comparator.comparing(Type::getTypeName);
                break;
            case "Calorie":
                typeComparator = Comparator.comparingDouble(Type::getCaloriePerMinute);
                break;
        }
        return order.equals("Ascending") ? typeComparator : typeComparator.reversed();
    }

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
