package com.example.test.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.test.databaseFiles.entity.Type;
import com.example.test.databaseFiles.repository.TypeRepository;

import java.util.List;

public class TypeViewModal extends AndroidViewModel {

    private TypeRepository typeRepository;
    private List<Type> allTypes;

    public TypeViewModal(@NonNull Application application) {
        super(application);
        typeRepository = new TypeRepository(application);
        allTypes = typeRepository.getAllTypes();
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

    public Type findType(int typeID){
        return typeRepository.findType(typeID);
    }

    public List<Type> getAllTypes() {
        return allTypes;
    }
}
