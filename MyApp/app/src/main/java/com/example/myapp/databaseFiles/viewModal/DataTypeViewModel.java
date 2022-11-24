package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.repository.TypeRepository;

public class DataTypeViewModel extends AndroidViewModel {

    private TypeRepository typeRepository;
    private String typeName;
    private int userID;

    public DataTypeViewModel(@NonNull Application application) {
        super(application);
        typeRepository = new TypeRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        typeName = null;
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
