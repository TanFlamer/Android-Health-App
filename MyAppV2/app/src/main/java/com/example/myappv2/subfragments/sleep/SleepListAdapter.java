package com.example.myappv2.subfragments.sleep;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myappv2.R;

import java.util.List;

public class SleepListAdapter extends ArrayAdapter<SleepListItem> {

    public SleepListAdapter(Context context, int resource, List<SleepListItem> sleepListItemList){
        super(context, resource, sleepListItemList);
    }

    @SuppressLint("ViewHolder")
    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;
        if(currentItemView == null) currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sleep_list_item, parent, false);

        SleepListItem sleepListItem = getItem(position);

        TextView dateView = currentItemView.findViewById(R.id.sleepDate);
        TextView sleepView = currentItemView.findViewById(R.id.sleepTime);
        TextView wakeView = currentItemView.findViewById(R.id.wakeTime);
        TextView durationView = currentItemView.findViewById(R.id.sleepDuration);

        dateView.setText(sleepListItem.getDate());
        sleepView.setText(sleepListItem.getSleepTime());
        wakeView.setText(sleepListItem.getWakeTime());
        durationView.setText(String.valueOf(sleepListItem.getSleepDuration()));

        return currentItemView;
    }
}
