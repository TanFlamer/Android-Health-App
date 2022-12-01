package com.example.myapp.fragments.sport.sportList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.example.myapp.R;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.type.Type;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportListAdapter extends BaseExpandableListAdapter {

    private final Context context;
    private final List<Sport> sportList;
    private final HashMap<Sport, List<Pair<Type, Integer>>> typeSports;
    private final HashMap<Sport, Boolean> buttonMap;
    private final SportListViewModel sportListViewModel;

    public SportListAdapter(Context context, HashMap<Sport, List<Pair<Type, Integer>>> typeSports, SportListViewModel sportListViewModel){
        this.context = context;
        this.sportList = new ArrayList<>(typeSports.keySet());
        this.typeSports = typeSports;
        this.sportListViewModel = sportListViewModel;
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
        View currentItemView = view;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        initialiseGroupView(currentItemView, i);
        return currentItemView;
    }

    public void initialiseGroupView(View view, int position){
        Sport sport = sportList.get(position);
        initialiseGroupDate(view, sport);
        initialiseHiddenLayout(view, sport);
        initialiseEditButton(view, sport);
        initialiseDeleteButton(view, sport);
    }

    public void initialiseGroupDate(View view, Sport sport){
        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(String.valueOf(sport.getDate()));
    }

    public void onLongClick(int position){
        Sport sport = sportList.get(position);
        buttonMap.put(sport, Boolean.FALSE.equals(buttonMap.get(sport)));
        notifyDataSetChanged();
    }

    public void initialiseHiddenLayout(View view, Sport sport){
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sport)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseEditButton(View view, Sport sport){
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        clickEdit.setOnClickListener(v -> context.startActivity(sportListViewModel.sportEdit(sport.getDate())));
    }

    public void initialiseDeleteButton(View view, Sport sport){
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view1 -> sportListViewModel.deleteSportList(context, sport).show());
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);

        initialiseChildView(currentItemView, i, i1);
        return currentItemView;
    }

    public void initialiseChildView(View view, int parent, int child){
        Pair<Type, Integer> pair = Objects.requireNonNull(typeSports.get(sportList.get(parent))).get(child);
        Type type = pair.first;
        int duration = pair.second;
        initialiseChildName(view, type);
        initialiseChildDuration(view, duration);
        initialiseCalorieView(view, type, duration);
    }

    public void initialiseChildName(View view, Type type){
        TextView nameView = view.findViewById(R.id.sportName);
        nameView.setText(type.getTypeName());
    }

    public void initialiseChildDuration(View view, int duration){
        TextView durationView = view.findViewById(R.id.sportDuration);
        durationView.setText(String.valueOf(duration));
    }

    public void initialiseCalorieView(View view, Type type, int duration){
        TextView calorieView = view.findViewById(R.id.sportCalorie);
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));
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
        sportListViewModel.sortSportLists(sportList, typeSports, data, order);
        for(Sport sport : sportList) buttonMap.put(sport, false);
        notifyDataSetChanged();
    }
}
