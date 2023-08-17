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

import com.example.myapp.R;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.subActivities.type.TypeDataActivity;

import java.util.HashMap;
import java.util.List;

public class SportTypeAdapter extends ArrayAdapter<Type> {

    private final List<Type> typeList;
    private final Context context;
    private final SportTypeViewModel sportTypeViewModel;
    private final HashMap<Type, Boolean> buttonMap;

    //constructor for sport type list adapter
    public SportTypeAdapter(@NonNull Context context, int resource, List<Type> typeList, SportTypeViewModel sportTypeViewModel) {
        super(context, resource, typeList);
        this.sportTypeViewModel = sportTypeViewModel;
        this.context = context;
        this.typeList = typeList;
        buttonMap = new HashMap<>();
        for(Type type : typeList) buttonMap.put(type, false);
    }

    @NonNull
    @Override //get view for each sport type
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for sport type if null
        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sport_list_item, parent, false);

        //initialise sport type view data
        initialiseAll(currentItemView, position);
        //return sport type view
        return currentItemView;
    }

    //initialise sport type view data
    public void initialiseAll(View view, int position){
        Type type = typeList.get(position);
        //initialise sport type hidden layout
        initialiseHiddenLayout(view, type);
        //get sport type name
        initialiseNameView(view, type);
        //get sport type calorie
        initialiseCalorieView(view, type);
        //initialise sport type edit button
        initialiseEditButton(view, type);
        //initialise sport type delete button
        initialiseDeleteButton(view, type);
    }

    //initialise sport type hidden layout
    public void initialiseHiddenLayout(View view, Type type){
        //get hidden layout by ID
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    //get sport type name
    public void initialiseNameView(View view, Type type){
        //get text view ID for sport type name
        TextView typeView = view.findViewById(R.id.sportType);
        //set sport type name
        typeView.setText(type.getTypeName());
    }

    //get sport type calorie
    public void initialiseCalorieView(View view, Type type){
        //get text view ID for sport type calorie
        TextView energyView = view.findViewById(R.id.sportEnergy);
        //set sport type calorie
        energyView.setText(String.valueOf(type.getCaloriePerMinute()));
    }

    //initialise sport type edit button
    public void initialiseEditButton(View view, Type type){
        //get edit button by ID
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        //go to edit sport type activity on click
        clickEdit.setOnClickListener(v -> {
            Intent intent = new Intent(context, TypeDataActivity.class);
            intent.putExtra("typeName", type.getTypeName());
            context.startActivity(intent);
        });
    }

    //initialise sport type delete button
    public void initialiseDeleteButton(View view, Type type){
        //get delete button by ID
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        //show dialog to validate sport type deletion on click
        clickDelete.setOnClickListener(view1 -> sportTypeViewModel.deleteSportType(context, type).show());
    }

    //show or hide hidden layout on long click
    public void onLongClick(int position){
        //get sport type on long click position
        Type type = typeList.get(position);
        //invert hidden layout visibility
        buttonMap.put(type, Boolean.FALSE.equals(buttonMap.get(type)));
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    //update sport type list when sport type list changes
    public void updateTypeList(List<Type> newTypeList, String data, String order){
        //clear old sport type list
        typeList.clear();
        //add new sport type list
        typeList.addAll(newTypeList);
        //sort new sport type list
        sortTypeList(data, order);
    }

    //sort sport type list
    public void sortTypeList(String data, String order){
        //sort sport type list
        sportTypeViewModel.sortTypeList(typeList, data, order);
        //hide hidden layout for all sport types
        for(Type type : typeList) buttonMap.put(type, false);
        //notify adapter dataset changed
        notifyDataSetChanged();
    }
}
