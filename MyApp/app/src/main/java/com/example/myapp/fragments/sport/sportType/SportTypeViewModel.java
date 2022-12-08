package com.example.myapp.fragments.sport.sportType;

import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;

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

    //constructor for sport type view model
    public SportTypeViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        typeRepository = mainApplication.getTypeRepository();
        int userID = mainApplication.getUserID();
        typeList = typeRepository.getAllTypes(userID);
    }

    //dialog to validate sport type deletion
    public AlertDialog deleteSportType(Context context, Type type){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> {
                    typeRepository.delete(type);
                    updateSaveLogs("Sport type " + type.getTypeName() + " deleted");
                })
                .setNegativeButton("No", null)
                .create();
    }

    //return live data of sport type list
    public LiveData<List<Type>> getTypeList(){
        return typeList;
    }

    //sort sport type list
    public void sortTypeList(List<Type> typeList, String data, String order){
        Comparator<Type> typeComparator = getComparator(data, order);
        typeList.sort(typeComparator);
    }

    //get comparator to sort sport type list
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

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
