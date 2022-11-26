package com.example.myapp.subActivities.type;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;

public class DataTypeViewModel extends AndroidViewModel {

    private TypeRepository typeRepository;
    private Type type;
    private int userID;


    public DataTypeViewModel(@NonNull Application application) {
        super(application);
        typeRepository = new TypeRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void loadType(String typeName){
        type = typeName == null ? null : typeRepository.findType(userID, typeName).get(0);
    }

    public boolean validateTypeName(String typeName){
        return typeRepository.findType(userID, typeName).size() == 0;
    }

    public void insert(String typeName, double calorie){
        typeRepository.insert(new Type(typeName, calorie, userID));
    }

    public void update(String typeName, double calorie){
        typeRepository.update(new Type(type.getTypeID(), typeName, calorie, userID));
    }

    public void delete(){
        typeRepository.delete(type);
    }

    public Type getType() {
        return type;
    }
}
