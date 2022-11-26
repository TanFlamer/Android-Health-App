package com.example.myapp.fragments.sport.sportList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<Sport> sportList;
    private HashMap<Sport, List<Pair<Type, Integer>>> typeSports;

    public SportExpandableListAdapter(Context context, HashMap<Sport, List<Pair<Type, Integer>>> typeSports){
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
        long sportDate = sportList.get(i).getDate();

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(String.valueOf(sportDate));

        return view;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        Pair<Type, Integer> typeDurationPair = Objects.requireNonNull(typeSports.get(sportList.get(i))).get(i1);
        Type type = typeDurationPair.first;
        int duration = typeDurationPair.second;

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);

        TextView nameView = view.findViewById(R.id.sportName);
        TextView durationView = view.findViewById(R.id.sportDuration);
        TextView calorieView = view.findViewById(R.id.sportCalorie);

        nameView.setText(type.getTypeName());
        durationView.setText(String.valueOf(duration));
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateSportList(HashMap<Sport, List<Pair<Type, Integer>>> newTypeSports){
        sportList.clear();
        sportList.addAll(newTypeSports.keySet());
        typeSports.clear();
        typeSports.putAll(newTypeSports);
        notifyDataSetChanged();
    }
}
