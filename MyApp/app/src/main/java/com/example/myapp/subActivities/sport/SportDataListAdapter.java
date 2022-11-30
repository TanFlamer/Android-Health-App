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

    private Context context;
    private List<Pair<Pair<Type, Integer>, Boolean>> typeSports;

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

        Pair<Pair<Type, Integer>, Boolean> initialPair = typeSports.get(position);

        Pair<Type, Integer> typeSport = initialPair.first;
        Type type = typeSport.first;
        int duration = typeSport.second;

        TextView typeView = currentItemView.findViewById(R.id.sportDataType);
        TextView durationView = currentItemView.findViewById(R.id.sportDataDuration);
        TextView calorieView = currentItemView.findViewById(R.id.sportDataCalorie);

        typeView.setText(type.getTypeName());
        durationView.setText(String.valueOf(duration));
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));

        Boolean selected = initialPair.second;
        currentItemView.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);

        return currentItemView;
    }
}
