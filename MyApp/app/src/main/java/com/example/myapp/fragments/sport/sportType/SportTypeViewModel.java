package com.example.myapp.fragments.sport.sportType;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;

import java.util.List;

public class SportTypeViewModel extends AndroidViewModel {

    private TypeRepository typeRepository;
    private LiveData<List<Type>> typeList;
    private int userID;

    public SportTypeViewModel(@NonNull Application application) {
        super(application);
        typeRepository = ((MainApplication) getApplication()).getTypeRepository();
        userID = ((MainApplication) getApplication()).getUserID();
        typeList = typeRepository.getAllTypes(userID);
    }

    public void insert(Type type){
        typeRepository.insert(type);
    }

    public void update(Type type){
        typeRepository.update(type);
    }

    public void delete(Type type){
        typeRepository.delete(type);
    }

    public Type findType(int userID, String typeName){
        return typeRepository.findType(userID, typeName);
    }

    public LiveData<List<Type>> getTypeList(){
        return typeList;
    }

    public int getUserID() {
        return userID;
    }
}
