package com.example.myapp.fragmentsSport.expandableListSport;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

import com.example.myapp.R;

import java.time.LocalDate;
import java.util.List;

public class SportExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<SportExpandableListItem> sportExpandableListItemList;

    public SportExpandableListAdapter(Context context, List<SportExpandableListItem> sportExpandableListItemList){
        this.context = context;
        this.sportExpandableListItemList = sportExpandableListItemList;
    }

    @Override
    public int getGroupCount() {
        return sportExpandableListItemList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return sportExpandableListItemList.get(i).getSportData().size();
    }

    @Override
    public Object getGroup(int i) {
        return sportExpandableListItemList.get(i).getSportData();
    }

    @Override
    public Object getChild(int i, int i1) {
        return sportExpandableListItemList.get(i).getSportData().get(i1);
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
        LocalDate sportDate = sportExpandableListItemList.get(i).getDate();

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item, null);

        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(sportDate.toString());

        return view;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        SportExpandableListData sportExpandableListData = sportExpandableListItemList.get(i).getSportData().get(i1);

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.sport_expandable_list_item_data, null);

        TextView nameView = view.findViewById(R.id.sportName);
        TextView durationView = view.findViewById(R.id.sportDuration);
        TextView calorieView = view.findViewById(R.id.sportCalorie);

        nameView.setText(sportExpandableListData.getName());
        durationView.setText(String.valueOf(sportExpandableListData.getDuration()));
        calorieView.setText(String.valueOf(sportExpandableListData.getCalories()));

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateSportList(List<SportExpandableListItem> newSportExpandableListItemList){
        sportExpandableListItemList.clear();
        sportExpandableListItemList.addAll(newSportExpandableListItemList);
        notifyDataSetChanged();
    }
}
