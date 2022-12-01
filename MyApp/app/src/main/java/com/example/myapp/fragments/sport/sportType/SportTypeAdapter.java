package com.example.myapp.fragments.sport.sportType;

import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.subActivities.type.TypeDataActivity;

import java.util.HashMap;
import java.util.List;

public class SportTypeAdapter extends ArrayAdapter<Type> {

    private final List<Type> typeList;
    private final Context context;
    private final SportTypeViewModel sportTypeViewModel;
    private final HashMap<Type, Boolean> buttonMap;

    public SportTypeAdapter(@NonNull Context context, int resource, List<Type> typeList, SportTypeViewModel sportTypeViewModel) {
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

        initialiseAll(currentItemView, position);
        return currentItemView;
    }

    public void initialiseAll(View view, int position){
        Type type = typeList.get(position);
        initialiseHiddenLayout(view, type);
        initialiseNameView(view, type);
        initialiseCalorieView(view, type);
        initialiseEditButton(view, type);
        initialiseDeleteButton(view, type);
    }

    public void initialiseHiddenLayout(View view, Type type){
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseNameView(View view, Type type){
        TextView typeView = view.findViewById(R.id.sportType);
        typeView.setText(type.getTypeName());
    }

    public void initialiseCalorieView(View view, Type type){
        TextView energyView = view.findViewById(R.id.sportEnergy);
        energyView.setText(String.valueOf(type.getCaloriePerMinute()));
    }

    public void initialiseEditButton(View view, Type type){
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> {
            Intent intent = new Intent(context, TypeDataActivity.class);
            intent.putExtra("typeName", type.getTypeName());
            context.startActivity(intent);
        });
    }

    public void initialiseDeleteButton(View view, Type type){
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view1 -> new AlertDialog.Builder(getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> sportTypeViewModel.delete(type))
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void onLongClick(int position){
        Type type = typeList.get(position);
        buttonMap.put(type, Boolean.FALSE.equals(buttonMap.get(type)));
        notifyDataSetChanged();
    }

    public void updateTypeList(List<Type> newTypeList, String data, String order){
        typeList.clear();
        typeList.addAll(newTypeList);
        sortTypeList(data, order);
    }

    public void sortTypeList(String data, String order){
        sportTypeViewModel.sortTypeList(typeList, data, order);
        for(Type type : typeList) buttonMap.put(type, false);
        notifyDataSetChanged();
    }
}
