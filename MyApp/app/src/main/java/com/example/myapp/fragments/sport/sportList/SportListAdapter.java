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
        if(currentItemView == null)
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        //initialise sport data view data
        initialiseGroupView(currentItemView, i);
        //return sport data view
        return currentItemView;
    }

    //initialise sport data view data
    public void initialiseGroupView(View view, int position){
        Sport sport = sportList.get(position);
        //initialise sport data date
        initialiseGroupDate(view, sport);
        //initialise sport data hidden layout
        initialiseHiddenLayout(view, sport);
        //initialise sport data edit button
        initialiseEditButton(view, sport);
        //initialise sport data delete button
        initialiseDeleteButton(view, sport);
    }

    //initialise sport data date
    public void initialiseGroupDate(View view, Sport sport){
        //get text view ID for sport data date
        TextView dateView = view.findViewById(R.id.sportDate);
        //convert long to local date
        LocalDate date = Instant.ofEpochMilli(sport.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
        //set sport data date
        dateView.setText(date.toString());
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

    //initialise sport data hidden layout
    public void initialiseHiddenLayout(View view, Sport sport){
        //get hidden layout by ID
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(sport)) ? View.VISIBLE : View.GONE);
    }

    //initialise sport data edit button
    public void initialiseEditButton(View view, Sport sport){
        //get edit button by ID
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        //send to edit sport data activity on click
        clickEdit.setOnClickListener(v -> context.startActivity(sportListViewModel.sportEdit(sport.getDate())));
    }

    //initialise sport data delete button
    public void initialiseDeleteButton(View view, Sport sport){
        //get delete button by ID
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        //show dialog to validate sport data deletion on click
        clickDelete.setOnClickListener(view1 -> sportListViewModel.deleteSportList(context, sport).show());
    }

    @SuppressLint("InflateParams")
    @Override //get view for each sport type
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        View currentItemView = view;

        //inflate new view for sport type if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);
        }

        //initialise sport type view data
        initialiseChildView(currentItemView, i, i1);
        //return sport type view
        return currentItemView;
    }

    //initialise sport type view data
    public void initialiseChildView(View view, int parent, int child){
        Pair<Type, Integer> pair = Objects.requireNonNull(typeSports.get(sportList.get(parent))).get(child);
        Type type = pair.first;
        int duration = pair.second;
        //get sport type name
        initialiseChildName(view, type);
        //get sport type duration
        initialiseChildDuration(view, duration);
        //get sport type calorie
        initialiseCalorieView(view, type, duration);
    }

    //get sport type name
    public void initialiseChildName(View view, Type type){
        //get text view ID for sport type name
        TextView nameView = view.findViewById(R.id.sportName);
        //set sport type name
        nameView.setText(type.getTypeName());
    }

    //get sport type duration
    public void initialiseChildDuration(View view, int duration){
        //get text view ID for sport type duration
        TextView durationView = view.findViewById(R.id.sportDuration);
        //set sport type duration
        durationView.setText(String.valueOf(duration));
    }

    //get sport type calorie
    public void initialiseCalorieView(View view, Type type, int duration){
        //get text view ID for sport type calorie
        TextView calorieView = view.findViewById(R.id.sportCalorie);
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
        //remove null sport types
        removeNull();
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

    //remove null sport types
    public void removeNull(){
        for(List<Pair<Type, Integer>> pairList : typeSports.values()){
            pairList.removeIf(pair -> pair == null || pair.first == null || pair.second == null);
        }
    }
}
