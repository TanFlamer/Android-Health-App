package com.example.myapp.subActivities.sport;

import android.content.Context;
import android.graphics.Color;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;

import java.util.List;

public class SportDataListAdapter extends BaseAdapter {

    private final Context context;
    private final List<Pair<Pair<Type, Integer>, Boolean>> typeSports;

    public SportDataListAdapter(@NonNull Context context, List<Pair<Pair<Type, Integer>, Boolean>> typeSports) {
        this.context = context;
        this.typeSports = typeSports;
    }

    @Override
    public int getCount() {
        return typeSports.size();
    }

    @Override
    public Object getItem(int position) {
        return typeSports.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.data_sport_list_item, parent, false);

        initialiseAll(currentItemView, position);
        return currentItemView;
    }

    public void initialiseAll(View view, int position){
        Pair<Pair<Type, Integer>, Boolean> pair = typeSports.get(position);
        initialiseTypeView(view, pair.first.first);
        initialiseDurationView(view, pair.first.second);
        initialiseCalorieView(view, pair.first.first, pair.first.second);
        initialiseSelectedView(view, pair.second);
    }

    public void initialiseTypeView(View view, Type type){
        TextView typeView = view.findViewById(R.id.sportDataType);
        typeView.setText(type.getTypeName());
    }

    public void initialiseDurationView(View view, int duration){
        TextView durationView = view.findViewById(R.id.sportDataDuration);
        durationView.setText(String.valueOf(duration));
    }

    public void initialiseCalorieView(View view, Type type, int duration){
        TextView calorieView = view.findViewById(R.id.sportDataCalorie);
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));
    }

    public void initialiseSelectedView(View view, boolean selected){
        view.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);
    }
}
