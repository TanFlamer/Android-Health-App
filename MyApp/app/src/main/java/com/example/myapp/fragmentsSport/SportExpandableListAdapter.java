package com.example.myapp.fragmentsSport;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.entity.Type;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<Sport> sportList;
    private HashMap<Sport, List<Pair<Type, Duration>>> typeSports;

    public SportExpandableListAdapter(Context context, HashMap<Sport, List<Pair<Type, Duration>>> typeSports){
        this.context = context;
        this.sportList = new ArrayList<>(typeSports.keySet());
        this.typeSports = typeSports;
    }

    @Override
    public int getGroupCount() {
        return sportList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return Objects.requireNonNull(typeSports.get(sportList.get(i))).size();
    }

    @Override
    public Object getGroup(int i) {
        return typeSports.get(sportList.get(i));
    }

    @Override
    public Object getChild(int i, int i1) {
        return Objects.requireNonNull(typeSports.get(sportList.get(i))).get(i1);
    }

    @Override
    public long getGroupId(int i) {
        return i;
    }

    @Override
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        LocalDate sportDate = sportList.get(i).getDate();

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(sportDate.toString());

        return view;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        Pair<Type, Duration> typeDurationPair = Objects.requireNonNull(typeSports.get(sportList.get(i))).get(i1);
        Type type = typeDurationPair.first;
        Duration duration = typeDurationPair.second;

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);

        TextView nameView = view.findViewById(R.id.sportName);
        TextView durationView = view.findViewById(R.id.sportDuration);
        TextView calorieView = view.findViewById(R.id.sportCalorie);

        nameView.setText(type.getName());
        durationView.setText(duration.toString());
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration.toMinutes()));

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateSportList(HashMap<Sport, List<Pair<Type, Duration>>> newTypeSports){
        sportList.clear();
        sportList.addAll(newTypeSports.keySet());
        typeSports.clear();
        typeSports.putAll(newTypeSports);
        notifyDataSetChanged();
    }
}
