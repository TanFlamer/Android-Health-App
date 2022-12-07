package com.example.myapp.subActivities.type;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;

public class TypeDataViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final TypeRepository typeRepository;
    private final int userID;
    private Type type;


    public TypeDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        typeRepository = mainApplication.getTypeRepository();
        userID = mainApplication.getUserID();
    }

    public void loadType(String typeName){
        type = typeName == null ? null : typeRepository.findType(userID, typeName);
    }

    public boolean validateTypeName(String typeName){
        return typeRepository.findType(userID, typeName) == null;
    }

    public void insert(String typeName, double calorie){
        updateSaveLogs("Sport Type " + typeName + " added");
        typeRepository.insert(new Type(typeName, calorie, userID));
    }

    public void update(String typeName, double calorie){
        updateSaveLogs("Sport Type " + typeName + " updated");
        type.setTypeName(typeName);
        type.setCaloriePerMinute(calorie);
        typeRepository.update(type);
    }

    public Type getType() {
        return type;
    }

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
