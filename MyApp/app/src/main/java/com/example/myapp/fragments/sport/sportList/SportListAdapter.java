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
import com.example.myapp.ViewHolder;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
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

    //constructor for sport data list adapter
    public SportListAdapter(Context context, HashMap<Sport, List<Pair<Type, Integer>>> typeSports, SportListViewModel sportListViewModel){
        this.context = context;
        this.sportList = new ArrayList<>(typeSports.keySet());
        this.typeSports = typeSports;
        this.sportListViewModel = sportListViewModel;
        buttonMap = new HashMap<>();
        for(Sport sport : sportList) buttonMap.put(sport, false);
    }

    @Override //get number of sport data
    public int getGroupCount() {
        return sportList.size();
    }

    @Override //get sport type count of sport data
    public int getChildrenCount(int i) {
        return Objects.requireNonNull(typeSports.get(sportList.get(i))).size();
    }

    @Override //get sport data
    public Object getGroup(int i) {
        return typeSports.get(sportList.get(i));
    }

    @Override //get sport type from sport data
    public Object getChild(int i, int i1) {
        return Objects.requireNonNull(typeSports.get(sportList.get(i))).get(i1);
    }

    @Override //get sport data ID
    public long getGroupId(int i) {
        return i;
    }

    @Override //get sport type ID
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override //check if ID stable
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override //get view for each sport data
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for sport data if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.sportDate));
            //add layout to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.layoutHidden));
            //add button to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.clickEdit));
            //add button to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.clickDelete));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //get view holder
        ViewHolder viewHolder = (ViewHolder) currentItemView.getTag();
        //update sport data view data
        updateGroupView(viewHolder, i);
        //return sport data view
        return currentItemView;
    }

    //update sport data view data
    public void updateGroupView(ViewHolder viewHolder, int position){
        Sport sport = sportList.get(position);
        //update sport data date
        updateGroupDate(viewHolder, sport);
        //update sport data hidden layout
        updateHiddenLayout(viewHolder, sport);
        //update sport data edit button
        updateEditButton(viewHolder, sport);
        //update sport data delete button
        updateDeleteButton(viewHolder, sport);
    }

    //update sport data date
    public void updateGroupDate(ViewHolder viewHolder, Sport sport){
        //get text view ID for sport data date
        TextView dateView = (TextView) viewHolder.getView(R.id.sportDate);
        //convert long to local date
        LocalDate date = Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
        //set sport data date
        dateView.setText(date.toString());
    }

    //update sport data hidden layout
    public void updateHiddenLayout(ViewHolder viewHolder, Sport sport){
        //get hidden layout by ID
        LinearLayout layoutHidden = (LinearLayout) viewHolder.getView(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sport)) ? View.VISIBLE : View.GONE);
    }

    //update sport data edit button
    public void updateEditButton(ViewHolder viewHolder, Sport sport){
        //get edit button by ID
        ImageView clickEdit = (ImageView) viewHolder.getView(R.id.clickEdit);
        //send to edit sport data activity on click
        clickEdit.setOnClickListener(v -> context.startActivity(sportListViewModel.sportEdit(sport.getDate())));
    }

    //update sport data delete button
    public void updateDeleteButton(ViewHolder viewHolder, Sport sport){
        //get delete button by ID
        ImageView clickDelete = (ImageView) viewHolder.getView(R.id.clickDelete);
        //show dialog to validate sport data deletion on click
        clickDelete.setOnClickListener(view1 -> sportListViewModel.deleteSportList(context, sport).show());
    }

    //show or hide hidden layout on long click
    public void onLongClick(int position){
        //get sport data on long click position
        Sport sport = sportList.get(position);
        //invert hidden layout visibility
        buttonMap.put(sport, Boolean.FALSE.equals(buttonMap.get(sport)));
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    @SuppressLint("InflateParams")
    @Override //get view for each sport type
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for sport type if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.sportName));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.sportDuration));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.sportCalorie));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //get view holder
        ViewHolder viewHolder = (ViewHolder) currentItemView.getTag();
        //update sport type view data
        updateChildView(viewHolder, i, i1);
        //return sport type view
        return currentItemView;
    }

    //update sport type view data
    public void updateChildView(ViewHolder viewHolder, int parent, int child){
        Pair<Type, Integer> pair = Objects.requireNonNull(typeSports.get(sportList.get(parent))).get(child);
        //get sport type
        Type type = pair.first;
        //get sport type duration
        int duration = pair.second;
        //update sport type name
        updateChildName(viewHolder, type);
        //update sport type duration
        updateChildDuration(viewHolder, duration);
        //update sport type calorie
        updateCalorieView(viewHolder, type, duration);
    }

    //update sport type name
    public void updateChildName(ViewHolder viewHolder, Type type){
        //get text view ID for sport type name
        TextView nameView = (TextView) viewHolder.getView(R.id.sportName);
        //set sport type name
        nameView.setText(type.getTypeName());
    }

    //update sport type duration
    public void updateChildDuration(ViewHolder viewHolder, int duration){
        //get text view ID for sport type duration
        TextView durationView = (TextView) viewHolder.getView(R.id.sportDuration);
        //set sport type duration
        durationView.setText(String.valueOf(duration));
    }

    //update sport type calorie
    public void updateCalorieView(ViewHolder viewHolder, Type type, int duration){
        //get text view ID for sport type calorie
        TextView calorieView = (TextView) viewHolder.getView(R.id.sportCalorie);
        //set sport type calorie
        calorieView.setText(String.valueOf(type.getCaloriePerMinute() * duration));
    }

    @Override //check if sport type selectable
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    //update sport data list when sport data or sport type list changes
    public void updateSportList(HashMap<Sport, List<Pair<Type, Integer>>> newTypeSports, String data, String order){
        //clear old sport data list
        sportList.clear();
        //add new sport data list
        sportList.addAll(newTypeSports.keySet());
        //clear old sport type list
        typeSports.clear();
        //add new sport type list
        typeSports.putAll(newTypeSports);
        //sort sport data and sport type lists
        sortSportList(data, order);
    }

    //sort sport data and sport type lists
    public void sortSportList(String data, String order){
        //sort sport data and sport type lists
        sportListViewModel.sortSportLists(sportList, typeSports, data, order);
        //hide hidden layout for all sport data
        for(Sport sport : sportList) buttonMap.put(sport, false);
        //notify adapter dataset changed
        notifyDataSetChanged();
    }
}
