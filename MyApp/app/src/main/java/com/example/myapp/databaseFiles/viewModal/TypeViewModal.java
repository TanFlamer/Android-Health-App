package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.repository.TypeRepository;

import java.util.List;

public class TypeViewModal extends AndroidViewModel {

    private TypeRepository typeRepository;
    private LiveData<List<Type>> allTypes;

    public TypeViewModal(@NonNull Application application, int userID) {
        super(application);
        typeRepository = new TypeRepository(application);
        allTypes = typeRepository.getAllTypes(userID);
    }

    public void insert(Type type) {
        typeRepository.insert(type);
    }

    public void update(Type type) {
        typeRepository.update(type);
    }

    public void delete(Type type) {
        typeRepository.delete(type);
    }

    public List<Type> findType(int typeID){
        return typeRepository.findType(typeID);
    }

    public LiveData<List<Type>> getAllTypes() {
        return allTypes;
    }
}
