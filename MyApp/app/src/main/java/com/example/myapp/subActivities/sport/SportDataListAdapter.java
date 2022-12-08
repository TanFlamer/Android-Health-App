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

    //constructor for sport list adapter
    public SportDataListAdapter(@NonNull Context context, List<Pair<Pair<Type, Integer>, Boolean>> typeSports) {
        this.context = context;
        this.typeSports = typeSports;
    }

    @Override //get sport type item count
    public int getCount() {
        return typeSports.size();
    }

    @Override //get sport type at position
    public Object getItem(int position) {
        return typeSports.get(position);
    }

    @Override //get sport type item ID
    public long getItemId(int position) {
        return position;
    }

    @NonNull
    @Override //get view for each sport type
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for sport type if null
        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.data_sport_list_item, parent, false);

        //initialise sport type view data
        initialiseAll(currentItemView, position);
        //return sport type view
        return currentItemView;
    }

    //initialise sport type view data
    public void initialiseAll(View view, int position){
        Pair<Pair<Type, Integer>, Boolean> pair = typeSports.get(position);
        //get sport type name
        initialiseTypeView(view, pair.first.first);
        //get sport type duration
        initialiseDurationView(view, pair.first.second);
        //get sport type calorie
        initialiseCalorieView(view, pair.first.first, pair.first.second);
        //get sport type selected state
        initialiseSelectedView(view, pair.second);
    }

    //get sport type name
    public void initialiseTypeView(View view, Type type){
        //get text view ID for sport type name
        TextView typeView = view.findViewById(R.id.sportDataType);
        //set sport type name
        typeView.setText(type.getTypeName());
    }

    //get sport type duration
    public void initialiseDurationView(View view, int duration){
        //get text view ID for sport type duration
        TextView durationView = view.findViewById(R.id.sportDataDuration);
        //set sport type duration
        durationView.setText(String.valueOf(duration));
    }

    //get sport type calorie
    public void initialiseCalorieView(View view, Type type, int duration){
        //get text view ID for sport type calorie
        TextView calorieView = view.findViewById(R.id.sportDataCalorie);
        //set sport type calorie
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));
    }

    //get sport type selected state
    public void initialiseSelectedView(View view, boolean selected){
        //set sport type background colour depending on selected state
        view.setBackgroundColor(selected ? Color.LTGRAY : Color.WHITE);
    }
}
