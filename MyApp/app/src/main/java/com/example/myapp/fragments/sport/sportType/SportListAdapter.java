package com.example.myapp.fragments.sport.sportType;

import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.subActivities.music.DataMusic;
import com.example.myapp.subActivities.type.DataType;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class SportListAdapter extends ArrayAdapter<Type> {

    private List<Type> typeList;
    Context context;
    SportTypeViewModel sportTypeViewModel;
    LinearLayout layoutVisible, layoutHidden;
    ImageView clickEdit, clickDelete;
    HashMap<Type, Boolean> buttonMap;

    public SportListAdapter(@NonNull Context context, int resource, List<Type> typeList, SportTypeViewModel sportTypeViewModel) {
        super(context, resource, typeList);
        this.sportTypeViewModel = sportTypeViewModel;
        this.context = context;
        this.typeList = typeList;
        buttonMap = new HashMap<>();
        for(Type type : typeList) buttonMap.put(type, false);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sport_list_item, parent, false);

        Type type = typeList.get(position);
        initialiseLayouts(currentItemView, position);
        initialiseData(currentItemView, type);
        initialiseEditButton(currentItemView, type);
        initialiseDeleteButton(currentItemView, type);
        return currentItemView;
    }

    public void initialiseLayouts(View currentItemView, int position){
        Type type = typeList.get(position);
        layoutVisible = currentItemView.findViewById(R.id.layoutVisible);
        layoutHidden = currentItemView.findViewById(R.id.layoutHidden);
        layoutVisible.setOnLongClickListener(v -> {
            buttonMap.put(type, Boolean.FALSE.equals(buttonMap.get(type)));
            notifyDataSetChanged();
            return true;
        });
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseData(View currentItemView, Type type){
        TextView typeView = currentItemView.findViewById(R.id.sportType);
        TextView energyView = currentItemView.findViewById(R.id.sportEnergy);

        typeView.setText(type.getTypeName());
        energyView.setText(String.valueOf(type.getCaloriePerMinute()));
    }

    public void initialiseEditButton(View currentItemView, Type type){
        clickEdit = currentItemView.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> {
            Intent intent = new Intent(context, DataType.class);
            intent.putExtra("typeName", type.getTypeName());
            context.startActivity(intent);
        });
        clickDelete = currentItemView.findViewById(R.id.clickDelete);
    }

    public void initialiseDeleteButton(View currentItemView, Type type){
        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> sportTypeViewModel.delete(type))
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void updateTypeList(List<Type> newTypeList, String data, String order){
        typeList.clear();
        typeList.addAll(newTypeList);
        sortTypeList(data, order);
    }

    public void sortTypeList(String data, String order){
        typeList.sort(getComparator(data, order));
        for(Type type : typeList) buttonMap.put(type, false);
        notifyDataSetChanged();
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
}
