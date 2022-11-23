package com.example.myapp.subActivities;

import android.content.Context;
import android.graphics.Color;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.BaseAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Type;

import java.time.Duration;
import java.util.List;

public class SportDataListAdapter extends BaseAdapter {

    private Context context;
    private List<Pair<Type, Duration>> typeSports;

    public SportDataListAdapter(@NonNull Context context, List<Pair<Type, Duration>> typeSports) {
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
        return 0;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.data_sport_list_item, parent, false);

        currentItemView.setBackgroundColor(Color.WHITE);

        Pair<Type, Duration> typeSport = typeSports.get(position);
        Type type = typeSport.first;
        Duration duration = typeSport.second;

        TextView typeView = currentItemView.findViewById(R.id.sportDataType);
        TextView durationView = currentItemView.findViewById(R.id.sportDataDuration);
        TextView calorieView = currentItemView.findViewById(R.id.sportDataCalorie);

        typeView.setText(type.getName());
        durationView.setText(duration.toString());
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration.toMinutes()));

        return currentItemView;
    }
}
