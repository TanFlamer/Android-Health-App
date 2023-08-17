package com.example.myapp.subActivities.type;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;

public class TypeDataViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final TypeRepository typeRepository;
    private final int userID;
    private Type type;


    //constructor for view model
    public TypeDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        typeRepository = mainApplication.getTypeRepository();
        userID = mainApplication.getUserID();
    }

    //load sport type from database
    public void loadType(String typeName){
        type = typeName == null ? null : typeRepository.findType(userID, typeName);
    }

    //check if sport type name taken
    public boolean validateTypeName(String typeName){
        return typeRepository.findType(userID, typeName) == null;
    }

    //insert new sport type to database
    public void insert(String typeName, double calorie){
        updateSaveLogs("Sport Type " + typeName + " with " + calorie + " calories per minute added");
        typeRepository.insert(new Type(typeName, calorie, userID));
    }

    //update existing sport type in database
    public void update(String typeName, double calorie){
        //check if sport type name different
        String oldTypeName = type.getTypeName();
        if(!oldTypeName.equals(typeName)){
            type.setTypeName(typeName);
            updateSaveLogs("Sport Type " + oldTypeName + " changed to " + typeName);
        }
        //check if sport type calorie per minute different
        double oldCalories = type.getCaloriePerMinute();
        if(Double.compare(oldCalories, calorie) != 0){
            type.setCaloriePerMinute(calorie);
            updateSaveLogs("Sport Type " + typeName + " updated to " + calorie + " calories per minute");
        }
        //update sport type
        typeRepository.update(type);
    }

    //return sport type
    public Type getType() {
        return type;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
