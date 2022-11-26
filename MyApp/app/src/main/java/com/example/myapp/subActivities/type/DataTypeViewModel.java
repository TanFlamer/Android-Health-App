package com.example.myapp.subActivities.type;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;

public class DataTypeViewModel extends AndroidViewModel {

    private TypeRepository typeRepository;

    private int userID;
    private int typeID;
    private String typeName;

    public DataTypeViewModel(@NonNull Application application) {
        super(application);
        typeRepository = new TypeRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void insert(String typeName, Double calorie){
        typeRepository.insert(new Type(typeName, calorie, userID));
    }

    public void update(String typeName, Double calorie){
        typeRepository.update(new Type(typeID, typeName, calorie, userID));
    }

    public void delete(String typeName, Double calorie){
        typeRepository.delete(new Type(typeID, typeName, calorie, userID));
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public boolean validateTypeName(String typeName){
        return typeRepository.findType(userID, typeName).size() == 0;
    }
}
