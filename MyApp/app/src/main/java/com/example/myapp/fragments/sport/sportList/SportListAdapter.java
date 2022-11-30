package com.example.myapp.fragments.sport.sportList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.subActivities.sport.SportDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<Sport> sportList;
    private HashMap<Sport, List<Pair<Type, Integer>>> typeSports;
    private HashMap<Sport, Boolean> buttonMap;
    private SportListFragment sportListsFragment;

    private LinearLayout layoutVisible, layoutHidden;
    private ImageView clickEdit, clickDelete;

    public SportListAdapter(Context context, HashMap<Sport, List<Pair<Type, Integer>>> typeSports, SportListFragment sportListsFragment){
        this.context = context;
        this.sportList = new ArrayList<>(typeSports.keySet());
        this.typeSports = typeSports;
        this.sportListsFragment = sportListsFragment;
        buttonMap = new HashMap<>();
        for(Sport sport : sportList) buttonMap.put(sport, false);
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
        Sport sport = sportList.get(i);

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(String.valueOf(sport.getDate()));
        initialiseLayouts(view, sport, i);
        initialiseButtons(view, sport);

        return view;
    }

    public void initialiseLayouts(View view, Sport sport, int position){
        layoutVisible = view.findViewById(R.id.layoutVisible);
        layoutHidden = view.findViewById(R.id.layoutHidden);
        layoutVisible.setOnLongClickListener(v -> {
            buttonMap.put(sport, Boolean.FALSE.equals(buttonMap.get(sport)));
            notifyDataSetChanged();
            return true;
        });
        layoutVisible.setOnClickListener(v -> {
            if(sportListsFragment.getExpandableListView().isGroupExpanded(position))
                sportListsFragment.getExpandableListView().collapseGroup(position);
            else
                sportListsFragment.getExpandableListView().expandGroup(position);
        });
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sport)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseButtons(View view, Sport sport){
        LocalDate date = Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
        clickEdit = view.findViewById(R.id.clickEdit);
        clickDelete = view.findViewById(R.id.clickDelete);
        clickEdit.setOnClickListener(v -> {
            Intent intent = new Intent(context, SportDataActivity.class);
            intent.putExtra("year", date.getYear());
            intent.putExtra("month", date.getMonthValue());
            intent.putExtra("day", date.getDayOfMonth());
            context.startActivity(intent);
        });
        clickDelete.setOnClickListener(view1 -> new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> sportListsFragment.getSportListViewModel().deleteSport(sport))
                .setNegativeButton("No", null)
                .create()
                .show());
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

    public void updateSportList(HashMap<Sport, List<Pair<Type, Integer>>> newTypeSports, String data, String order){
        sportList.clear();
        sportList.addAll(newTypeSports.keySet());
        typeSports.clear();
        typeSports.putAll(newTypeSports);
        sortSportList(data, order);
    }

    public void sortSportList(String data, String order){
        sportList.sort(getSportComparator(data, order));
        for(List<Pair<Type, Integer>> pairList : typeSports.values()) pairList.sort(getTypeComparator(data, order));
        for(Sport sport : sportList) buttonMap.put(sport, false);
        notifyDataSetChanged();
    }

    public Comparator<Sport> getSportComparator(String data, String order){
        Comparator<Sport> sportComparator = Comparator.comparingLong(Sport::getDate);
        switch (data) {
            case "Date Added":
                sportComparator = Comparator.comparingInt(Sport::getSportID);
                break;
            case "Sport Date":
                sportComparator = Comparator.comparingLong(Sport::getDate);
                break;
            case "Calories":
                sportComparator = Comparator.comparingDouble(this::getCalories);
                break;
        }
        return order.equals("Ascending") ? sportComparator : sportComparator.reversed();
    }

    public Comparator<Pair<Type, Integer>> getTypeComparator(String data, String order){
        Comparator<Pair<Type, Integer>> typeComparator = Comparator.comparing(a -> a.first.getTypeName());
        switch (data) {
            case "Date Added":
                typeComparator = Comparator.comparingInt(a -> a.first.getTypeID());
                break;
            case "Name":
                typeComparator = Comparator.comparing(a -> a.first.getTypeName());
                break;
            case "Calories":
                typeComparator = Comparator.comparingDouble(a -> a.first.getCaloriePerMinute() * a.second);
                break;
        }
        return order.equals("Ascending") ? typeComparator : typeComparator.reversed();
    }

    public double getCalories(Sport sport){
        double calories = 0;
        for(Pair<Type, Integer> pair : Objects.requireNonNull(typeSports.get(sport)))
            calories += pair.first.getCaloriePerMinute() * pair.second;
        return calories;
    }
}
